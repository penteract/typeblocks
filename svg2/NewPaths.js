"use strict";
/*

PADDINGH
├──┤
┌─────────────┐ ┬
│    border   │ │ PADDINGV
│  ┌───────┐  │ ┴
│  │       │  │
│  │content│  │
│  │       │  │
│  └───────┘  │
│             │
└─────────────┘
content may have an effective size of 0.
border graphics should be contained within the border region.


relevant constants from Layout.js:┌
const PADDINGH = 8
const PADDINGV = 8
font size is 16


Assumptions:
The border graphics will be an SVG path that encloses the content

Options for conveying meaning:
Color (currently used to indicate what belongs to what, but imperfectly)
animation (polymorphism?)
  This has now been implemented, but looks pretty bad
    (too small to see clearly
    ,dramatic increase in power use
    ,whether something is being hovered over can now change without the mouse moving)
  one alternative is to substitue in all valid concrete types sequentially
    This makes sense with typeclasses - cycle through instances.
      This can be done to give a sequence in which each concrete type matching the typeclass appears infinitly often,
        even though there may be infinitely many such concrete types
      With multiple typeclasses, is there a better idea than generating from 1 and testing against others?
      How to handle typeclass combinations with no instances?
stylistic differences (eg curves vs straight edges and sharp corners)
Loops in the border indicating polymorphic types that may be arrow types (and so may have holes)
  these should be in the bottom right corner
Base types should have rotational symmetry (order 4 when the box is square)

Decisions to make:
what should be a 'default' border?
- minimal rectangle around content (non-holes have jigsaw protrusions)
    - problematic if the content has size 0
    - the right kind of protrusions feel like 'part of the shape'
- maximal border inside the region (holes have jigsaw protrusions)
    - Things appear bigger
    - could make types feel like 'missing bits' which is misleading
- in the middle
    - allows both protrusions and intrusions
    - protrusions and intrusions would be smaller, making it harder to see details
      - interesting interlocking patterns that do both would allow the same level of detail
Corner embellishments or edge embellishments?
- Edge embellishments are easier to combine sensibly
  - put them adjacent for pairs or perhaps Either
  - for type constructors * -> * (lists, etc), vertical edges can be used for
    the constructor and horizontal edges for the argument
- corners could be considered part of vertical edges
- how to treat corners when
How to scale?
- It's important that boxes of different sizes can be recognised as having the same (or different) types
- Stretching along the whole length
  - the thickness of the border is constrained by a constant, so it can't be scaled
  - distortions arise - something may go from looking like a needle, to a semicircle, to a gentle hump
- Adding straight segments
- Adding straight segments, but doing a bit of scaling for very small cases
  - How should corners work if horizontal and vertical are scaled by different amounts?
    - scale them to match the smaller
  - should things move outwards?
  - this allows things to look bigger, but may make margins appear incorrect, particularly for non-square small boxes.
- Change stroke thickness?


Options for graphics:
Finite list of hand-constructed shapes
  - This might not be bad for builtin types when the shapes have some meaning
    - eg shape that looks like square brackets for lists
Procedural generation
  - can lead to similar looking things
    such similarity would have no semantic relevance
  - can lead to janky looking things e.g. self-intersections
  - assuming a finite combinatorial design (e.g. 5 parts each of which can be 10 different things)
    there's a tradeoff between the following:
    - the size of the state space (too small and shapes get reused)
    - the similarity between the closest points in the state space
      (too similar and you can't tell different things apart easily)
    - Nothing looks broken; it all scales reasonably
*/


// Edge description format:
// describe a path from -4,0 to 4,0 not leaving the box -4,-4 -- 4,4 which could be the top edge for a box
// path may be scaled
function sym(halfpath) {
  let p = parsePath("L -4 0 " + halfpath)
  return parsePath(unparse(p) + " " + unparse(transformPath(flip(p), [[-1, 0, 0], [0, -1, 0]])))
}
const straightEdge = parsePath("L -4 0 L 4 0")
const point = parsePath("L -4 0 L 0 -4 L 4 0")
const lump = parsePath("L -4 0 A 4 4 0 0 1 4 0")
const square = parsePath("L -2 0 L -2 -4 L 2 -4 L 2 0")
const ssym = sym("L -4 -4 L 0 -4")
const tsym = sym("L -4 -4")
const tsym2 = sym("L 0 -4")
const tsym3 = sym("L -2 -4")
const csym = parsePath("L -4 0 A 2 4 0 0 1 0 0 A 2 4 0 0 0 4 0")
const down = parsePath("L -4 4 L 4 4")
const shapes = [point, lump, square, csym, down, ssym, tsym, tsym2, tsym3]


// Corner description:
// corners go from 0,4 to 4,0 not leaving the box -4,-4 -- 4,4

const corner = parsePath("L 0 4 L 0 0 L 4 0")
const circled = parsePath("L 0 4 A 4 4 0 1 1 4 0")
const rounded = parsePath("L 0 4 A 4 4 0 0 1 4 0")
const indent = parsePath("L 0 4 A 4 4 0 0 0 4 0")
const stair = parsePath("L 0 4 L 4 4 L 4 0")
const cornershapes = [corner, circled, indent, stair]


function sym2(halfpath) {
  let p = parsePath(halfpath)
  return parsePath(unparse(p) + " " + unparse(transformPath(flip(p), [[0, 1, 0], [1, 0, 0]])))
}

// a corner for a polymorphic type that might represent an antihole
const maybeantihole = sym2("L 0 4 C 0 3.5 -7 5 1 1 C 3 0 8 2 5 5")

const themaybeantihole = sym2("L 1 1 C 3 0 8 2 5 5")

SVGGElement.prototype.drawBox = function() {
let pathEl = this.children[0]
  let type = this.baseType
  if (this.baseType == " defn ") {
    pathEl.setAttribute("d",simplePath(0, 0, this.width, this.height, straightEdge, corner))
    return;
  }
  else if(type[0]==type[0].toLowerCase()){
    let h = hash(type)
    let ps = animatedPaths([2,4,8][h%3])
    h=(h/3)|0
    let skew = ((h%5)-2)/2
    let vals = ""
    for (let p of ps){
      let parsed = parsePath(p)
      parsed = transformPath(parsed,[[1,skew,0],[0,1,0]])
      vals += lessSimplePath(0, 0, this.width, this.height, [parsed,parsed,parsed,parsed], [corner,maybeantihole,corner,corner]) + ";"
    }
    pathEl.innerHTML=`<animate dur="1s" attributeName="d" repeatCount="indefinite" values="${vals}"/>`
  }else{
    let path = shapes[hash(type) % shapes.length]
    //let corpath = cornershapes[hash(type) % cornershapes.length]
    pathEl.setAttribute("d",simplePath(0, 0, this.width, this.height, path, corner))
  }
}


function simplePath(x, y, width, height, edgePath, corPath) {
  return lessSimplePath(x,y,width,height,[edgePath,edgePath,edgePath,edgePath],[corPath,corPath,corPath,corPath])
}
function lessSimplePath(x, y, width, height, edgePaths, corPaths) {
  const MORESPACING = true
  let scale = Math.min(width, height, PADDINGH * 3) / (PADDINGH * 3)
  if (MORESPACING) scale = Math.min(width, height, PADDINGH * 4) / (PADDINGH * 4)
  if (scale < 0.5) alert("error, scale not in expected range" + scale)
  // if scale is less than 1, pretend there's something in the middle

  let hmar = Math.min(width / 6, PADDINGH / 2)
  let vmar = Math.min(height / 6, PADDINGV / 2)
  if (MORESPACING) hmar = Math.min(width / 8, PADDINGH / 2)
  if (MORESPACING) vmar = Math.min(height / 8, PADDINGH / 2)
  let minmar = Math.min(hmar, vmar)
  /*
  let hmar = PADDINGH/2
  let vmar = PADDINGV/2
  */

  let left = [x, 1]
  let right = [x + width, -1]
  let top = [y, 1]
  let bottom = [y + height, -1]
  let pts = function(i) {
    let [xz, dx] = [left, right][((i + 3) & 4) / 4]
    let [yz, dy] = [top, bottom][((i + 1) & 4) / 4]
    return [
      xz + dx * (hmar + (((i + 2) >> 1) & 1) * minmar),
      yz + dy * (vmar + ((i >> 1) & 1) * minmar)]
  }
  let path = "M" + pts(0)
  for (let i = 0; i < 8; i += 2) {
    path += mkEdge(pts(i), pts(i + 1), scale, edgePaths[i/2])
    path += mkCor(pts(i + 1), pts(i + 2), scale, corPaths[i/2])
  }
  return path + " Z"
}
//Make an edge from p1 to p2 clockwise
function mkEdge(p1, p2, scale, path) {
  let midpoint = p1.add(p2).div(2)
  let dir = p2.sub(p1)
  dir = dir.div(norm(dir))
  return unparse(transformPath(path, toMatrix(midpoint, dir, scale))) + "L" + p2
}
function mkCor(p1, p2, scale, path) {
  let dir = [[0.5, -0.5], [0.5, 0.5]].mcol(p2.sub(p1))
  let midpoint = p2.sub(dir)
  dir = dir.div(norm(dir))
  return unparse(transformPath(path, toMatrix(midpoint, dir, scale)))
}
function toMatrix(translate, direction, scale) {
  let [dx, dy] = direction.div(norm(direction)).mul(scale)
  return [
    [dx, -dy, translate[0]],
    [dy, dx, translate[1]]
  ]
}