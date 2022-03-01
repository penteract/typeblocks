
const epsilon = 0.0001

function recClose(l1, l2) {
  if (!isNaN(l1)) {
    return (l1 + epsilon > l2 && l1 - epsilon < l2)
  }
  if (Array.isArray(l1) && (l1.length === l2.length)) {
    for (let i in l1) {
      if (!recClose(l1[i], l2[i])) {
        return false
      }
    }
    return true
  }
  else return l1 === l2
}
function testf(code, expected) {
  try {
    let res = eval(code)

    if (!recClose(res, expected)) {
      console.log(code, "results in ", res, " not ", expected)
    }
  } catch{
    console.log("error running ", code, " expected", expected)
  }
}

function test(code, expected) {
  try {
    let res = eval(code)
    if (res !== expected) {
      console.log(code, "results in ", res, " not ", expected)
    }
  } catch{
    console.log("error running ", code, " expected", expected)
  }
}
let M = [[2, 10, 200], [8, 23, 5000]]
testf("[[2,10,200],[8,23,5000]].mcol([6,10,1])", [312, 5278])
testf("[[2,10,200],[8,23,5000]].mm([[6,1],[10,20],[0,0]])", [[112, 202], [278, 468]])
testf("svd([[Math.cos(1),-Math.sin(1)],[Math.sin(1),Math.cos(1)]].mm([[8,0],[0,4]]))", [[8, 4], 180 / Math.PI])