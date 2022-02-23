const SVGNS = "http://www.w3.org/2000/svg"

// Add a text node during box construction
SVGGElement.prototype.addText = function(text) {
  let txt = document.createElementNS(SVGNS, "text")
  txt.innerHTML = text
  txt.freewidth = txt.width = getLength(text)
  txt.height = 10
  this.appendChild(txt)
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
  g.numOwned = g.children.length - 1
  //Appearance
  g.style.fill = fill
  g.style.stroke = stroke
  //g.style.borderImageSource = `url(data:image/svg+xml;base64,${btoa(borderSVG(type, fill, stroke))})`
  //g.setxy(0, 0)
  if (!g.isHole) {
    g.draggable = true
  }
  if (typeof cols[0] != "string") {
    cols[0][2] += 5 //restore mutated value
  }
  // Warning: borderImage is buggy in firefox in various ways.
  // Consider reconstructing svg whenever the box's dimensions change

  //Structure
  g.scope = parent
  parent.appendChild(g)
  g.scopeIndex = parent.children.length - 1
  //if(!g.isHole) g.addEventListener("touchstart",startDrag(g))
  //if(!g.isHole) g.addEventListener("mousedown",startDrag(g))
  //parent.floating.push(g)
  //everything.push(g)
  dirty.push(g)
  return g
}
//const everything=[]
function makeBox(text, type) {
  let c = Math.random() * 360
  let fill = [c, 100, 80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
  let stroke = hsluv.hsluvToHex([c, 50, 50])//`hsl(${c},50%,70%)`
  dirty = []
  let g = subBox(text, type, [fill, stroke], ["#DDD", "#BBB"], false, svg)
  redraw(dirty)
  return g
}
SVGGElement.prototype.drawBox = function() {
  this.children[0].setAttribute("d", `M 0 0 h ${this.width} v ${this.height} h ${-this.width} Z`)
}