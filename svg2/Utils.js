"use strict";
// General Purpose

function hash(str) {
  let n = 0
  for (let i = 0; i < str.length; i++) {
    n += str.charCodeAt(i)
    n = (n * 31) % 1000000007
  }
  return n
}

// Vector/matrix operations
Array.prototype.mul = function(x) { return this.map(y => y * x) }
Array.prototype.div = function(x) { return this.map(y => y / x) }
Array.prototype.add = function(v) { return this.map((y, i) => y + v[i]) }
Array.prototype.sub = function(v) { return this.map((y, i) => y - v[i]) }
Array.prototype.norm = function() { return Math.sqrt(this.reduce((a, b) => a + b * b, 0)) }
function norm(v) {
  return Math.sqrt(v.reduce((a, b) => a + b * b, 0))
}
function det2(m) {// determinant of 2x2 matrix
  return m[0][0] * m[1][1] - m[0][1] * m[1][0]
}
Array.prototype.mcol = function(v) {
  return this.map(row => row.map((x, i) => x * v[i]).reduce((a, b) => a + b, 0))
}
Array.prototype.mm = function(m) {
  return this.map(row => m[0].map((_, j) => row.map((x, i) => x * m[i][j]).reduce((a, b) => a + b, 0)))
}

const SVGNS = "http://www.w3.org/2000/svg"

function createSVGElement(tag) {
  return document.createElementNS(SVGNS, tag)
}

// Project Specific
SVGGElement.prototype.getClientXY = function() {
  let m = this.parentElement.getCTM()
  return [this.xPos * m.a + this.yPos * m.c + m.e, this.xPos * m.b + this.yPos * m.d + m.f]
}
SVGGElement.prototype.toLocalCoords = function(x, y) {
  let m = this.getCTM().inverse()
  return [x * m.a + y * m.c + m.e, x * m.b + y * m.d + m.f]
}
SVGGElement.prototype.setClientXY = function(x, y) {
  let m = this.parentElement.getCTM().inverse()
  this.setPos(x * m.a + y * m.c + m.e, x * m.b + y * m.d + m.f)
}
SVGGElement.prototype.changeClientXY = function(dx, dy) {
  let m = this.parentElement.getCTM().inverse()
  this.setPos(this.xPos + dx * m.a + dy * m.c, this.yPos + dx * m.b + dy * m.d)
}

function isArg(g) {
  return g.parentElement.filled === g
}
function isDefn(g) {//TODO: update for svgs
  return g.classList.contains("defn")
}
function isFixed(g){
  return g.isLHS || g.isImport
}

SVGGElement.prototype.boxes = function*() {
  for (let c of this.children) {
    if (c.nodeName === this.nodeName) {
      yield c;
    }
  }
}
SVGGElement.prototype.ascend = function(f) {
  if (!this.isTopLevel && f(this)) {
    if (this.parentElement === null) console.log(this)
    this.parentElement.ascend(f)
  }
}
SVGGElement.prototype.visit = function(f) {
  f(this)
  let subNodes = [...this.boxes()]
  for (let c of subNodes) c.visit(f)
}

//Type stuff
//Strong match: exact match
//Weak match: return types match
function typeEq(s, t) {
  return JSON.stringify(s) == JSON.stringify(t)
}

function checkMatch(s, t) {
  let [sargs, sres] = decompose(s)
  let [targs, tres] = decompose(t)
  return sres == tres
}
function decompose(t) {//given a type, return a pair consisting of a list of arguments and a result
  let args = []
  while (!isBase(t)) {
    var [arg, t] = t
    args.push(arg)
  }
  return [args, t]
}

//check if a shape as currently filled lines up perfectly with a hole (and the things floating in it)
function isPerfectMatch(shape, hole) {
  if (!tyEq(shape.boxType, hole.boxType)) return false
  let innerHoles = getHoles(shape)
  let pairs = [[shape, hole]]
  let i = 0
  for (let arg of hole.boxes()) {
    if (i >= innerHoles.length) return false
    let m = isPerfectMatch(arg, innerHoles[i++])
    if (!m) return false
    pairs = pairs.concat(m)
  }
  if (i < innerHoles.length) return false
  return pairs
}

//check if a final segment of a shape's holes line up with a final segment of a hole's floating terms
//Note: this is not recursive - for children it calls isPerfectMatch
function isNearPerfectMatch(shape, hole) {
  if (!tyEq(shape.boxType, hole.boxType)) return false
  let innerHoles = getHoles(shape)
  let pairs = [[shape, hole]]
  let i = innerHoles.length - 1
  for (let arg of Array(...hole.boxes()).reverse()) {
    if (i < 0) break
    let m = isPerfectMatch(arg, innerHoles[i--])
    if (!m) break
    pairs = pairs.concat(m)
  }
  return pairs
}

function getHoles(g) {
  if (g.isHole) throw "expected to be called on non-holes"
  let result = []
  for (let hole of g.boxes()) {
    let fill = hole.filled
    if (fill) result = result.concat(getHoles(fill))
    else result.push(hole)
  }
  return result
}

function getLength(txt){//TODO: sanitize
  testtext.innerHTML=txt
  return testtext.getComputedTextLength()
}