"use strict";
function hash(str){
  let n=0
  for (let i=0;i<str.length;i++){
    n=(n*31)%1000000007
    n+=str.charCodeAt(i)
  }
  return n
}

function typeEq(s,t){
  return JSON.stringify(s) == JSON.stringify(t)
}


//Type stuff
//Strong match: exact match
//Weak match: return types match

//given a type, return a pair consisting of a list of arguments and a result
function decompose(t){
  let args=[]
  while(!isBase(t)){
    var [arg,t] = t
    args.push(arg)
  }
  return [args,t]
}
function checkMatch(s,t){
  let [sargs,sres] = decompose(s)
  let [targs,tres] = decompose(t)
  return sres==tres
}

function isArg(g){
  return g.parentElement.filled===g
}


function getHoles(g){
  if(g.isHole) throw "expected to be called on non-holes"
  let result = []
  for (let hole of g.children){
    let fill = hole.filled
    if(fill) result = result.concat(getHoles(fill))
    else result.push(hole)
  }
  return result
}

//check if a shape as currently filled lines up perfectly with a hole (and the things floating in it)
function isPerfectMatch(shape,hole){
  if(shape.baseType!==hole.baseType)return false
  let innerHoles = getHoles(shape)
  let pairs = [[shape,hole]]
  let i = 0
  for(let arg of hole.children){
    if(i>=innerHoles.length) return false
    let m = isPerfectMatch(arg,innerHoles[i++])
    if(!m) return false
    pairs = pairs.concat(m)
  }
  if(i<innerHoles.length) return false
  return pairs
}

//check if a final segment of a shape's holes line up with a final segment of a hole's floating terms
//Note: this is not recursive - for children it calls isPerfectMatch
function isNearPerfectMatch(shape,hole){
  if(shape.baseType!==hole.baseType)return false
  let innerHoles = getHoles(shape)
  let pairs = [[shape,hole]]
  let i = innerHoles.length-1
  for(let arg of Array(...hole.children).reverse()){
    if(i<0) break
    let m = isPerfectMatch(arg,innerHoles[i--])
    if(!m) break
    pairs = pairs.concat(m)
  }
  return pairs
}
