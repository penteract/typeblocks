/* Duplication/deletion
relates to Initialization.js

Without using these methods, all terms constructed would be linear
*/
EPSILON=0.001

SVGGElement.prototype.duplicate = function(newPar) {
  if (isFixed(this)) {// just make a brand new box with the right type, colors and scope
    if (newPar === undefined) {
      newPar = this.scope
    }
    let cols = [this.style.fill, this.style.stroke]
    redrawDirty() // We don't want to redraw the newly created boxes normally
    // Only need to worry about typeScope here if we're copying a hole in an LHS
    let g = subBox(this.text, this.type, cols, ["#DDD", "#BBB"], false, newPar,false,this.isHole?this.typeScope:undefined)
    // newly created nodes have been added in order (deepest first)
    for (let n of dirty){
      calcFreeWidth(n)
    }
    dirty=[]
    if (!this.isHole) g.defn = this.defn
    if(this.isHole) {g.scopeIndex = this.scopeIndex}
    // overflow = Math.max(0, this.freewidth - maxwidth)
    // maxwidth = this.freewidth - this.overflow
    g.redraw(this.freewidth - this.overflow + EPSILON) // I'm not completely sure why EPSILON is needed.
      //Floating point imprecision could explain it, but it wasn't necessary until making an imports div.
      //CSS transforms might be linked.
    return g
  }

  let g = createSVGElement("g")
  if (newPar === undefined) {
    newPar = this.parentElement
  }
  newPar.appendChild(g)
  // Deal with scope and typeScope
  for (let prop of ["scope","typeScope"]){
    if (this[prop].contains(newPar)) {
      g[prop] = this[prop]
    }
    else {
      // Our (this's) [type]scope has already been duplicated.
      // The duplicated scope should be as many levels above g
      // as our scope is above us
      g[prop] = g
      let climbing = this
      while (climbing !== this[prop]) {
        if (climbing.isTopLevel) throw "Climbed too far"
        climbing = climbing.parentElement
        g[prop] = g[prop].parentElement
      }
    }
  }
  if(g.typeScope===g){
    g.tyVars={}
  }
  g.boxType = mkBoxType(this.boxType,g)
  //Deal with most of the properties
  //Not copied: isLHS/isImport (once copied, it is no longer LHS; also those shouldn't be here), some things related to layout
  for (let prop of [
    "type", "text", "isHole", "displayType", "scopeIndex", "numOwned", "defn", "isConstructor", "tmpCount",
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
      //TODO: unify types
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

SVGGElement.prototype.delete = function(started,quickDelete) {
  // Child node is responsible for removing self from parent
  if (started === undefined || quickDelete) {
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
    ch.delete(true,quickDelete)
  }

  if (this.toBeDeleted) {
    if (this.filled) throw "Trying to delete something that's not empty(filled)"
    for (let x of this.boxes()) throw "Trying to delete something that's not empty"
    if(isPolyVar(this.boxType)){
      this.boxType.var.uses.delete(this)
      if(!quickDelete && this.boxType.var.tmp){checkNeeded(this.boxType.var)}
    }
    if(!quickDelete)detach(this)
    this.remove() //does this do anything if detatch happened?
  }
  else {
    if (snapTo !== this.parentElement) {
      makeFloating(this, snapTo)
    }
  }
}
