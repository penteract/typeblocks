'use strict';
/*
Graphics related properties of a box:

freewidth: the width a box would be without line wrapping

width: the width of the box as computed by the last call to redraw
height: the height of the box ...
lines: tracks the locations of the child nodes, wrapped into lines. A list of
  line objects which track the top left of the line (x and y),
  its size (width and height) and the number of nodes on the line (count)
xPos,yPos: the top left corner of a box

cols: the fill and border color of the box
baseType: determines the shape of the box

*/

const MAXWIDTH = 640
const SPACINGH = 8
const PADDINGH = SPACINGH

const SPACINGV = SPACINGH
const PADDINGV = SPACINGV


SVGGElement.prototype.boxes = function*() {
  for (let c of this.children) {
    if (c.nodeName === this.nodeName) {
      yield c;
    }
  }
}
SVGGElement.prototype.ascend = function(f) {
  if (this !== svg && f(this)) {
    this.parentElement.ascend(f)
  }
}


/*
redrawing process:
while dragging, the layout should not shift
  a temporary node gets added to the svg, and other nodes may change appearence
  (eg become invisible) but may not change shape.
upon release, the user action (move, copy, delete) gets processed and results in
a change to the structure of the syntax tree. This change is completed before
any layout is recomputed.
While the structure is being changed, some nodes are marked as moved, and some
holes are marked as vacated.
These are topologically sorted and freewidths are updated (bottom up), marking
updated nodes as dirty. Dirty nodes are then redrawn top down, computing
maxwidth and descending into dirty nodes and nodes for which
overflow = max(0, freewidth-maxwidth) has changed since the last redraw
(theorem: if n.overflow changes, then n.parentElement.dirty or
n.parentElement.overflow changes)
*/

// dirty is a list of nodes, each of which is moved or vacated
function redraw(dirty) {
  // begin by counting the number of dirty children so we can do this properly
  for (let d of dirty) {
    d.ascend(function(n) {
      n.dirty += 1
      return n.dirty == 1;// go up if it's the first time we've visited this node
    })
  }
  // Now visit dirty nodes once each in topological order (bottom up) to determine free widths
  let topNodes = []
  for (let d of dirty) {
    d.ascend(function(n) {
      if (n.dirty > 1) {
        n.dirty -= 1
        return false
      }
      if (n.filled) {
        n.freewidth = n.filled.freewidth
      }
      else {
        n.freewidth = PADDINGH * 2
        for (let i = 1; i < n.children.length; i++) {
          n.freewidth += n.children[i].freewidth + SPACINGH * (i > 1)
        }
      }
      if (n.parentElement === svg) topNodes.push(n)
      return true
    })
  }
  // Now descend, recomputing layouts
  for (let t of topNodes) {
    t.redraw(MAXWIDTH)
  }
}


/*
Recomputes layout for a box if it needs updating. Descends into children where
needed.
*/
SVGGElement.prototype.redraw = function(maxwidth) {
  let overflow = Math.max(0, this.freewidth - maxwidth)
  if (overflow == this.overflow && this.dirty == 0) {
    return;
  }
  this.overflow = overflow
  this.dirty = 0
  if (this.filled) {
    this.filled.redraw(maxwidth)
    this.width = this.filled.width
    this.height = this.filled.height
    return;
  }

  // track the place that new things get put
  let line = { "x": PADDINGH, "y": PADDINGV, "width": 0, "height": 0, "count": 0 }
  let lines = []
  let maxw = 0
  // Add a finished line and set child positions
  let chcount = 1
  let cur = this
  function pushline(l) {
    lines.push(l)
    if (maxw < l.width) maxw = l.width;
    for (let i = chcount; i < chcount + l.count; i++) {
      let c = cur.children[i]
      c.yPos = l.y + ((l.height - c.height) / 2)
      c.setPos()
    }
    chcount += l.count
  }

  for (let c of this.children) if (c.nodeName.toLowerCase() != "path") {
    if (c.redraw) c.redraw(maxwidth - 2 * PADDINGH)
    if (line.count > 0 && line.x + line.width + SPACINGH + c.width > this.maxwidth - PADDINGH) {
      pushline(line)
      line = {
        "x": PADDINGH, "y": line.y + line.height + SPACINGV,
        "width": c.width, "height": c.height, "count": 1
      }
      c.xPos = PADDINGH
    }
    else {
      c.xPos = line.x + line.width + SPACINGH * (line.count !== 0)
      line.width = c.xPos - line.x + c.width
      if (c.height > line.height) line.height = c.height
      line.count += 1
    }
  }
  if (line.count > 0) {
    pushline(line)
  }
  this.lines = lines
  let changed = false
  let width = maxw + 2 * PADDINGH
  if (this.width !== width) {//floating point comparison. It would be nice to avoid this
    this.width = width
    changed = true
  }
  let height = line.y + line.height + PADDINGV
  if (this.height !== height) {//floating point comparison. It would be nice to avoid this
    this.height = height
    changed = true
  }
  if (changed) this.drawBox()
}

SVGTextElement.prototype.setWidth = function(maxwidth) {
  this.width = this.getComputedTextLength()
  this.height = PADDINGV * 2 //TODO: pick the right value
}
SVGGElement.prototype.setPos = function() {
  this.setAttribute("transform", `translate(${this.xPos},${this.yPos})`)
}
SVGTextElement.prototype.setPos = function() {
  this.setAttribute("transform", `translate(${this.xPos},${this.yPos + 12.8})`)
}