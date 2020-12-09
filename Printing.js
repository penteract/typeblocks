function makeVarname(lamIndex,ownerIndex){
  return `x${lamIndex}v${ownerIndex}`
}

let lamCount = 0
function printTerm(g){
  lamCount = 0
  return printRec(g)
}
function printRec(g){
  let result = ""
  let parenthesize = false
  if (g.isHole){
    g.lamIndex = lamCount++
    let numArgs = 0
    let hasFloating = false
    for(let i=0; i<g.floating.length; i++){
      let float = g.floating[i]
      if(float===undefined || (float.owner===g && float.ownerIndex===i)){
        numArgs += 1
      }
      if(float!==undefined){
        hasFloating = true
      }
    }
    if(numArgs>0){
      parenthesize = true
      result = "\\ "
      for (let i=0; i<numArgs; i++){
        result += makeVarname(g.lamIndex,i)+" "
      }
      result += "-> "
    }
    if(hasFloating || !g.filled){
      result +=  "_ " //Haskell hole indicating that something is incomplete.
      //Generated terms without these holes SHOULD NOT have type errors
    }
    if (g.filled){
      result += printRec(g.filled)
    }
    if (hasFloating){
      parenthesize=true
      for(let i=0; i<g.floating.length; i++){
        let float = g.floating[i]
        if(float!==undefined){
          result += printRec(float)
        }
      }
    }
  }
  else{
    if(!g.text){
      if (g.owner===svg){
        throw "top level boxes expected to have text"
      }
      let lamIndex = g.owner.lamIndex
      if(lamIndex===undefined){
        throw "Either printTerm was called on a non-top-level box or something has gone wrong"
      }
      result = makeVarname(lamIndex,g.ownerIndex) + " "
    }
    else{
      result = g.text + " "
    }
    let atArg = 0
    if(isInfix(g.text)){
      while(atArg<g.floating.length && atArg<2){
        let arg = g.floating[atArg++]
        if(atArg==1) result = printRec(arg)+" "+result
        else result+=printRec(arg)
      }
      result="("+result+")"
    }
    if(g.floating.length>atArg){
      parenthesize=true
      while(atArg<g.floating.length ){
        result+=printRec(g.floating[atArg++])
      }
    }
  }
  if(parenthesize){
    result=`(${result})`
  }
  return result
}
