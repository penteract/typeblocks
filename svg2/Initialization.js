"use strict";
// Initialization
function makeBox(text, type) {
  let c = Math.random() * 360
  let fill = [c, 100, 80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
  let stroke = hsluv.hsluvToHex([c, 50, 50])//`hsl(${c},50%,70%)`
  dirty = []
  let g = subBox(text, type, [fill, stroke], ["#DDD", "#0004"], false, root)
  redraw(dirty)
  g.setPos(0, 0)
  return g
}


let dirty = []
function subBox(text, type, cols, otherCols, isHole, parent, scopeIndex) {
  let [fill, stroke] = cols
  let g = createSVGElement("g")
  g.appendChild(createSVGElement("path"))
  //if (hide) g.classList.add("hidden")
  if (g.isHole = isHole) {
    //g.classList.add("hole")
    g.filled = false
  }
  if (typeof cols[0] != "string") {
    fill = hsluv.hsluvToHex(fill)
    cols[0][2] -= 5 // make nested parts darker
    // Possibly this is a mistake and it's more important to have
    // identical colors for recognising when things are from the same
    // top level object
  }
  //g.classList.add("box")
  if (text == "ifThenElse") { } else
    if (text && (!isInfix(text) || isBase(type))) { g.addText(text) }
  g.type = type
  g.text = text

  //Children
  let numOwned = 0
  while (!isBase(type)) {
    assertEq(type.length, 2)
    var [arg, type] = type
    if (text == "ifThenElse") { g.addText(["if", " then", " otherwise"][numOwned]) }
    let ch = subBox("", arg, otherCols, cols, !isHole, g, numOwned++)
    if (g.children.length == 2 && text && isInfix(text)) { g.addText(text) }
  }
  g.baseType = type
  g.numOwned = numOwned
  if (numOwned !== [...g.boxes()].length)
    throw `numOwned (${numOwned}) should be the number of subboxes at creation ${[...g.boxes()].length}`
  //Appearance
  g.style.fill = fill
  g.style.stroke = stroke
  if (typeof cols[0] != "string") {
    cols[0][2] += 5 //restore mutated value
  }

  //Structure
  g.scope = parent
  parent.appendChild(g)
  g.scopeIndex = scopeIndex
  if (!g.isHole) g.addEventListener("touchstart", startDrag(g))
  if (!g.isHole) g.addEventListener("mousedown", startDrag(g))
  //parent.floating.push(g)
  //everything.push(g)
  //Layout
  g.dirt = 0
  dirty.push(g)
  return g
}

// Add a text node during box construction
SVGGElement.prototype.addText = function(text) {
  let txt = createSVGElement("text")
  txt.innerHTML = text
  txt.freewidth = txt.width = getLength(text)
  txt.height = 16
  this.appendChild(txt)
}

// Duplication
SVGGElement.prototype.duplicate = function(newPar) {
  if (this.isLHS) {// just make a brand new box with the right type, colors and scope
    // TODO : check this works when defns are implemented
    //let hide = false
    if (newPar === undefined) {
      newPar = this.scope
      //hide = true
    }
    let cols = this.colors//[this.style.backgroundColor,this.style.borderColor]
    let g = subBox(this.text, this.type, cols, ["#DDD", "#BBB"], false, newPar)
    if (!this.isHole) g.defn = this.defn
    g.scopeIndex = this.scopeIndex
    return g
  }

  let g = createSVGElement("g")
  if (newPar === undefined) {
    newPar = this.parentElement
  }
  newPar.appendChild(g)
  // Deal with scope
  if (this.scope.contains(newPar)) {
    g.scope = this.scope
  }
  else {
    // Our (this's) scope has already been duplicated.
    // The duplicated scope should be as many levels above g
    // as our scope is above us
    g.scope = newPar
    let climbing = this.parentElement
    while (climbing !== this.scope) {
      if (climbing === root) throw "Climbed too far"
      climbing = climbing.parentElement
      g.scope = g.scope.parentElement
    }
  }
  //Deal with most of the properties
  for (let prop of [
    "type", "text", "isHole", "baseType", "scopeIndex", "numOwned", "defn", "isConstructor",
    "xPos", "yPos", "freewidth", "width", "height", "overflow", "lines", "dirty", "dirt"]) {
    g[prop] = this[prop]
  } for (let prop of ["fill", "stroke"]) {
    g.style[prop] = this.style[prop]
  } for (let prop of ["transform"]) {
    g.setAttribute(prop, this.getAttribute(prop))
  }
  for (let c of ["filled", "filling", "hole", "box"]) {
    if (this.classList.contains(c)) g.classList.add(c)
  }
  if (!g.isHole) g.addEventListener("touchstart", startDrag(g))
  if (!g.isHole) g.addEventListener("mousedown", startDrag(g))

  // Deal with the children
  for (let node of this.children) {
    let dupnode = node.duplicate(g)
    if (this.filled === node) {
      g.filled = dupnode
    }
  }
  return g
}

SVGPathElement.prototype.duplicate = function(newPar) {
  let p = createSVGElement("path")
  for (let prop of ["d"]) {
    p.setAttribute(prop, this.getAttribute(prop))
  }
  newPar.appendChild(p)
}
SVGTextElement.prototype.duplicate = function(newPar) {
  let t = createSVGElement("text")
  newPar.appendChild(t)
  for (let prop of ["transform"]) {
    t.setAttribute(prop, this.getAttribute(prop))
  } for (let prop of ["xPos", "yPos", "freewidth", "width", "height"]) {
    t[prop] = this[prop]
  }
  t.innerHTML = this.innerHTML
}

//TODO: rename file - deletion is not initialization, but it fits here
SVGGElement.prototype.delete = function(started) {
  // Child node is responsible for removing self from parent
  if (started === undefined) {
    this.toBeDeleted = true
  }
  else if (this.scope.toBeDeleted) {
    this.toBeDeleted = true
  }
  else {
    //TODO: make sure this behaves as expected for higher order types
    var snapTo = this.parentElement
    if (snapTo.toBeDeleted) {
      while (snapTo.toBeDeleted) {
        snapTo = snapTo.parentElement
      }
      if (snapTo === this.scope && snapTo !== root) {
        for (let sib of snapTo.children) {
          if (sib.scope === snapTo && sib.scopeIndex === this.scopeIndex) {
            // Consider not doing this  - it may delete things that are still
            // wanted, and the behaviour of deleting every copy of a variable
            // except the first may be found  surprising
            this.toBeDeleted = true
            break
          }
        }
      }
    }
    else {
      //Will snapback, but need to process children first
    }
  }
  for (let ch of Array.from(this.boxes())) {
    ch.delete(true)
  }

  if (this.toBeDeleted) {
    if (this.filled) throw "Trying to delete something that's not empty(filled)"
    for (let x of this.boxes()) throw "Trying to delete something that's not empty"
    detach(this, "g")
    this.remove() //does this do anything?
  }
  else {
    if (snapTo !== this.parentElement) {
      makeFloating(this, snapTo)
    }
  }
}