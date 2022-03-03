const SVGNS = "http://www.w3.org/2000/svg"

// Initialization
function makeBox(text, type) {
  let c = Math.random() * 360
  let fill = [c, 100, 80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
  let stroke = hsluv.hsluvToHex([c, 50, 50])//`hsl(${c},50%,70%)`
  dirty = []
  let g = subBox(text, type, [fill, stroke], ["#DDD", "#0004"], false, root)
  redraw(dirty)
  return g
}


let dirty = []
function subBox(text, type, cols, otherCols, isHole, parent) {
  let [fill, stroke] = cols
  let g = document.createElementNS(SVGNS, "g")
  g.appendChild(document.createElementNS(SVGNS, "path"))
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
  while (!isBase(type)) {
    assertEq(type.length, 2)
    var [arg, type] = type
    if (text == "ifThenElse") { g.addText(["if", " then", " otherwise"][g.children.length]) }
    let ch = subBox("", arg, otherCols, cols, !isHole, g)
    if (g.children.length == 2 && text && isInfix(text)) { g.addText(text) }
  }
  g.baseType = type
  g.numOwned = Array(g.boxes()).length
  //Appearance
  g.style.fill = fill
  g.style.stroke = stroke
  if (!g.isHole) {
    g.draggable = true
  }
  if (typeof cols[0] != "string") {
    cols[0][2] += 5 //restore mutated value
  }

  //Structure
  g.scope = parent
  parent.appendChild(g)
  g.scopeIndex = parent.children.length - 1
  if (!g.isHole) g.addEventListener("touchstart", startDrag(g))
  if (!g.isHole) g.addEventListener("mousedown", startDrag(g))
  //parent.floating.push(g)
  //everything.push(g)
  dirty.push(g)
  return g
}

// Add a text node during box construction
SVGGElement.prototype.addText = function(text) {
  let txt = document.createElementNS(SVGNS, "text")
  txt.innerHTML = text
  txt.freewidth = txt.width = getLength(text)
  txt.height = 16
  this.appendChild(txt)
}

// Duplication
HTMLDivElement.prototype.duplicate = function(newPar) {
  console.log("TODO")
  return;
  if (this.isLHS) {// just make a brand new box with the right type, colors and scope
    let hide = false
    if (newPar === undefined) {
      newPar = this.scope
      hide = true
    }
    let cols = this.colors//[this.style.backgroundColor,this.style.borderColor]
    let g = subBox(this.text, this.type, cols, ["#DDD", "#BBB"], false, newPar, hide)
    if (!this.isHole) g.defn = this.defn
    g.scopeIndex = this.scopeIndex
    return g
  }

  let g = document.createElementNS(SVGNS, "g")
  if (newPar === undefined) {
    newPar = this.parentElement
    //g.classList.add("hidden")
    //g.setxy(0,0)
    g.xPos = g.yPos = 0
  }
  else {
    g.xPos = this.xPos
    g.yPos = this.yPos
    //g.setxy(this.x,this.y)
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
    "type", "text", "isHole", "baseType", "scopeIndex", "draggable", "numOwned", "defn", "isConstructor"]) {
    g[prop] = this[prop]
  } for (let prop of [
    "backgroundColor", "borderColor", "borderImageSource"]) {
    g.style[prop] = this.style[prop]
  }
  for (let c of ["filled", "filling", "hole", "box"]) {
    if (this.classList.contains(c)) g.classList.add(c)
  }

  // Deal with the children
  for (let node of this.childNodes) {
    if (node.nodeType === Node.TEXT_NODE) {
      g.append(node.textContent)
    }
    else {
      let dupnode = node.duplicate(g)
      if (this.filled === node) {
        g.filled = dupnode
      }
    }
  }

  return g
}