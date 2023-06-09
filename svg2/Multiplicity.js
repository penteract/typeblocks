/* Duplication/deletion
relates to Initialization.js

Without using these methods, all terms constructed would be linear
*/

SVGGElement.prototype.duplicate = function(newPar) {
  if (this.isLHS) {// just make a brand new box with the right type, colors and scope
    if (newPar === undefined) {
      newPar = this.scope
    }
    let cols = [this.style.fill, this.style.stroke]
    redrawDirty() // We don't want to redraw the newly created boxes normally
    let g = subBox(this.text, this.type, cols, ["#DDD", "#BBB"], false, newPar)
    // newly created nodes have been added in order (deepest first)
    for (let n of dirty){
      calcFreeWidth(n)
    }
    dirty=[]
    if (!this.isHole) g.defn = this.defn
    g.scopeIndex = this.scopeIndex
    // overflow = Math.max(0, this.freewidth - maxwidth)
    // maxwidth = this.freewidth - this.overflow
    g.redraw(this.freewidth - this.overflow)
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
  //Not copied: isLHS (once copied, it is no longer LHS), some things related to layout
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
  p.innerHTML=this.innerHTML
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
