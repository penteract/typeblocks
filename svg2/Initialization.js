"use strict";
// Initialization

function makeDraggable(g) {
  g.addEventListener("touchstart", startDrag(g))
  g.addEventListener("mousedown", startDrag(g))
}
function makeDefn(text, type) {
  let defn = createSVGElement("g")
  defn.appendChild(createSVGElement("path"))
  defn.baseType = " defn "
  defn.classList.add("defn")
  defn.classList.add("box")
  makeDraggable(defn)
  let line = createSVGElement("g")
  line.appendChild(createSVGElement("path"))
  line.baseType = " line "// hack for display
  defn.appendChild(line)
  line.classList.add("line")
  // colors
  let c = Math.random() * 360
  let op = (c + 180) % 360
  let fill = [c, 100, 80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
  let stroke = hsluv.hsluvToHex([c, 50, 50])//`hsl(${c},50%,70%)`
  let opFill = [op, 100, 80]
  let opStroke = hsluv.hsluvToHex([op, 50, 50])

  line.lhs = subBox(text, type, [fill, stroke], [opFill, opStroke], false, line, true)
  line.addText("↦")//consider ⟼
  line.rhs = subBox("", type, ["#DDD", "#BBB"], [opFill, opStroke], true, line)
  for (let ch of line.lhs.boxes()) {
    ch.scope = line.rhs
    makeDraggable(ch)
  }
  makeDraggable(line.lhs)
  line.lhs.scope = root
  //line.lhs.colors = [fill, stroke]
  line.lhs.defn = defn
  line.lhs.visit(n => n.isLHS = true)
  defn.scope = root
  root.appendChild(defn)
  defn.setPos(0, 0)
  redrawDirty()

  return defn
}

function makeBox(text, type) {
  let c = Math.random() * 360
  let fill = [c, 100, 80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
  let stroke = hsluv.hsluvToHex([c, 50, 50])//`hsl(${c},50%,70%)`
  let g = subBox(text, type, [fill, stroke], ["#DDD", "#0004"], false, root)
  redrawDirty()
  g.setPos(0, 0)
  return g
}

function subBox(text, type, cols, otherCols, isHole, parent, noDrag) {
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
    let ch = subBox("", arg, otherCols, cols, !isHole, g, noDrag)
    ch.scopeIndex = numOwned++ // For indexing into g.mapsto during evaluation
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
  if (!noDrag && !g.isHole) {
    g.addEventListener("touchstart", startDrag(g))
    g.addEventListener("mousedown", startDrag(g))
  }
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
