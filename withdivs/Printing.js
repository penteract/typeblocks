"use strict";
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
    let numArgs = g.numOwned
    let hasFloating = false
    for(let ch of g.children){
      if(ch!==g.filled){
        hasFloating=true
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
      if (g.filled) result += "{- "
      else parenthesize=true
      for(let ch of g.children){
        if(ch!==g.filled){
          result += printRec(ch)
        }
      }
      if (g.filled) result += " -}";
    }
  }
  else{
    if (g.owner!==root){
      let lamIndex = g.owner.lamIndex
      if(lamIndex===undefined){
        throw "Either printTerm was called on a non-top-level box or something has gone wrong"
      }
      result = makeVarname(lamIndex,g.ownerIndex) + " "
    }

    let n=0
    let infix=false
    for(let node of g.childNodes){// This makes some assumptions about text nodes (eg there's only 1)
      n++
      if(node.nodeType==Node.TEXT_NODE){
        let text = node.textContent
        if(n==1){
          if(isInfix(text)) text="("+text+")"
        }
        else if(n==2){
          if(!isInfix(text)) text="`"+text+"`"
          infix=true
        }
        else throw ("text occurs after the second argument");
        result = result+" "+text
      }
      else{
        result = result+" "+printRec(node)
        if(n==3 && infix){
          result="("+result+")"
          parenthesize=false
        }
        else parenthesize=true
      }
    }
  }
  if(parenthesize){
    result=`(${result})`
  }
  return result
}
