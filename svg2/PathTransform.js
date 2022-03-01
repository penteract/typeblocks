/*
Code for transforming svg paths.
Does not intend to accept all valid svgs - it assumes single spaces separate tokens.
*/
argCounts = {
  M: 2, L: 2, C: 6, Q: 4, A: 7
}

// Take a string describing a path and convert it into a list of instructions
// Each instruction is a list containing a string, a starting position and a sequence of numbers
function parsePath(path) {
  let tokens = path.split(" ")
  instrs = []
  if (tokens[0] !== "L") throw "Path expected to start with L <startingpoint>"
  let prev = []
  for (let i = 0; i < tokens.length;) {
    let tok = tokens[i++]
    let curInstr = [tok, prev]
    let N = argCounts[tok]
    if (N === undefined) throw `Command expected at token "${tok}" (index ${i - 1}) when parsing ${path}`
    for (let j = 0; j < N; j++) {
      tok = tokens[i++]
      let val = tok - 0
      if (isNaN(val)) throw `Number expected at token "${tok}" (index ${i - 1}) when parsing ${path}`
      curInstr.push(val)
    }
    if (prev.length) instrs.push(curInstr) // don't push first instruction
    prev = curInstr.slice(curInstr.length - 2)
  }
  return instrs
}
function unparse(instrs) {
  let path = ["L " + instrs[0][1].join(" ")]
  for (let instr of instrs) {
    path.push(instr[0] + " " + instr.slice(2).join(" "))
  }
  return path.join(" ")
}
function unparseWhole(instrs) {
  let path = ["M " + instrs[0][1].join(" ")]
  for (let instr of instrs) {
    path.push(instr[0] + " " + instr.slice(2).join(" "))
  }
  return path.join(" ")
}

// Change the direction of a path
function flip(instrs) {
  let result = []
  for (let instr of instrs) {
    switch (instr[0]) {
      case "A": // Change sweep flag
        result.push(["A", instr.slice(instr.length - 2), instr.slice(2, 6), 1 - instr[6], ...instr[1]])
        break;
      default:
        let pts = []
        for (i = instr.length - 3; i > 2; i -= 2) {
          if (i % 2 == 0) throw "I can't count"
          pts.push(instr[i - 1], instr[i])
        }
        result.push([instr[0], instr.slice(instr.length - 2), ...pts, ...instr[1]])
    }
  }
  return result.reverse()
}

function svd(mat) {
  //https://lucidar.me/en/mathematics/singular-value-decomposition-of-a-2x2-matrix/
  let [[a, b], [c, d]] = mat
  let [a2, b2, c2, d2] = [a, b, c, d].map(x => x * x)
  let theta = Math.atan2(2 * a * c + 2 * b * d, a2 + b2 - c2 - d2) * 90 / Math.PI
  let S1 = a2 + b2 + c2 + d2
  let S2 = norm([(a2 + b2 - c2 - d2), 2 * (a * c + b * d)])
  return [[Math.sqrt((S1 + S2) / 2), Math.sqrt((S1 - S2) / 2)], theta]
}

/*
Transform a path (preparsed) according to a 2x3 matrix giving scaling, rotation and translation
TODO: consider skew, but this gets annoying for arcs (singular value decompositon)

*/
function transformPath(instrs, mat) {
  let result = []
  for (let instr of instrs) {
    switch (instr[0]) {
      case "A": // Change sweep flag
        let [rx, ry, rot] = instr.slice(2, 5)
        // rot is a rotation anticlockwise in degrees
        let cr = Math.cos(Math.PI * rot / 180)
        let sr = Math.sin(Math.PI * rot / 180)
        //let Mrot = [[cr,-sr], [sr,cr]]
        //let Msc = [[rx,0],[0,ry]]
        //M = Mrot.mm(Msc)
        // a matrix to transform the unit circle into the ellipse for the initial arc
        let M = [[rx * cr, -ry * sr], [rx * sr, ry * cr], [0, 0]]
        // decompose the matrix which transforms the unit circle into the ellipse for the transformed arc
        let [rxy, theta] = svd(mat.mm(M))
        let sweep = det2(mat) > 0 ? instr[6] : (1 - instr[6])
        result.push(["A", mat.mcol(instr[1].concat([1])), ...rxy, theta, instr.slice(5, 6), sweep,
          mat.mcol(instr.slice(instr.length - 2).concat([1]))])
        break;
      default:
        let pts = []
        for (i = 2; i < instr.length; i += 2) {
          pts.push(...mat.mcol([instr[i], instr[i + 1], 1]))
        }
        result.push([instr[0], mat.mcol(instr[1].concat([1])), ...pts])
    }
  }
  return result
}