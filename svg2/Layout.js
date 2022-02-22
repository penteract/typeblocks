'use strict';
/*
Graphics related properties of a box:

width: the width of the box as computed by the last call to childChanged
height: the height of the box ...
lines: tracks the locations of the child nodes, wrapped into lines. A list of
  line objects which track the top left of the line (x and y),
  its size (width and height) and the number of nodes on the line (count)

cols: the fill and border color of the box
baseType: determines the shape of the box

*/

const MAXWIDTH = 400
const SPACINGH = 5
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
      if(n.filled){
        n.freewidth=n.filled.freewidth
      }
      else{
        n.freewidth = PADDINGH * 2
        for (let i = 0; i < n.children.length; i++) {
          n.freewidth += n.children[i].freewidth + SPACINGH * (i > 0)
        }
      }
      if (n.parentElement === svg) topNodes.append(n)
      return true
    })
  }
  // Now descend, recomputing layouts
  for(let t of topNodes){
    t.redraw(MAXWIDTH)
  }
}


  /*
  To be called when a box has children added, removed, rearranged or resized
  Returns true and redraws the box if it changes size,
  Returns false otherwise
  */
  SVGGElement.prototype.childChanged = function(maxwidth) {
    /*let overflow = Math.max(0, this.freewidth-maxwidth)
    if (overflow == this.overflow && this.dirty==0){
      return;
    }
    this.overflow = overflow
    this.dirty = 0
    if(this.filled){
      this.filled.redraw(maxwidth)
      this.width=this.filled.width
      this.height=this.filled.height
    }*/

    // track the place that new things get put
    let line = { "x": PADDINGH, "y": PADDINGV, "width": 0, "height": 0, "count": 0 }
    let lines = []
    let maxw = 0

    for (let c of this.children) {
      if (line.count > 0 && line.x + line.width + SPACINGH + c.width > this.maxwidth - PADDINGH) {
        lines.push(line)
        if (maxw < line.width) maxw = line.width;
        line = {
          "x": PADDINGH, "y": line.y + height + SPACINGV,
          "width": c.width, "height": c.height, "count": 1
        }
      }
      else {
        line.width = line.width + SPACINGH * (line.count !== 0) + c.width
        if (c.height > line.height) line.height = c.height
        line.count += 1
      }
    }
    if (line.count > 0) {
      lines.push(line)
      if (maxw < line.width) maxw = line.width;
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
      this.height = width
      changed = true
    }
    if (changed) this.redraw()
    return changed
  }
  /*
  Sets the maximum width of a box.
  This should be called when a box is moved or its parent's maxwidth changes
  Also called for initialisation.
  Returns
  */
  SVGGElement.prototype.setWidth = function(maxwidth) {
    maxwidth ??= MAXWIDTH
  if (maxwidth === this.maxwidth || (this.width <= maxwidth && this.lines.length == 1)) {
      this.maxwidth = maxwidth
      return False
    }
    this.maxwidth = maxwidth
    if (this.filled) {
      this.filled.setWidth(maxwidth)
      let ret = this.width === this.filled.width && this.height === this.filled.height
      this.width = this.filled.width
      this.height = this.filled.height
      return ret
    }
    let subwidth = maxwidth - 2 * PADDINGH
    for (let c of this.children) {
      c.setWidth(subwidth)
    }

    return this.childChanged()
  }

  SVGTextElement.prototype.setWidth = function(maxwidth) {
    this.width = this.getComputedTextLength()
    this.height = 10 //TODO: pick the right value
  }
  SVGGElement.prototype.setPos = function(x, y) {
  }