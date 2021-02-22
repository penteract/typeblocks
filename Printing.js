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



function printReduced(g){
  lamCount = 0
  let t = buildTerm(g)
  t.reduce()
  return t.toString(false)
}

class Lambda{
  init(numArgs) {
    this.numArgs=numArgs
    this.argCounts=(new Array(this.numArgs)).fill(0)
    this.lamIndex=lamCount++
  }
  reduce(){
    this.body = this.body.reduce()
    let args,a
    while(this.argCounts[this.numArgs-1]
      && (args = this.body.args)
      && (a=args[args.length-1])
      && a.owner===this
      && a.argIndex===this.numArgs-1
    ){
      args.pop()
      this.argCounts.pop()
      this.numArgs--
    }
    this.body = this.body.reduce()
    if(this.numArgs==0) return this.body
    return this
  }
  toString(p){
    if(this.numArgs==0) return this.body.toString(p)
    else{
      let result = "\\ "
      for (let i=0; i<this.numArgs; i++){
        result += (this.argCounts[i]?makeVarname(this.lamIndex,i):"_")+" "

      }
      result += "-> "
      result += this.body.toString(false)
      return p?"("+result+")":result
    }
  }
}
class Constant {
  constructor(name) {
    this.name = name
  }
  reduce(){return this}
  toString(p){
    if (isInfix(this.name)){
      return "("+this.name+")"
    }
    return this.name
  }
}
class Application {
  constructor(head) {
    this.head = head;
    this.args = []
  }
  reduce(){
    this.head.reduce()
    for(let i=0;i<this.args.length;i++){
      this.args[i]=this.args[i].reduce()
    }
    if(this.args.length==0){
      return this.head
    }
    else{
      return this
    }
  }
  toString(p){
    if(this.args.length==0) return this.head.toString(p)
    else{
      let result=""
      let i=0
      if(this.head.name && isInfix(this.head.name)){
        if(this.args.length==1)
          return "("+this.args[0].toString(true)+this.head.name+")"
        result=this.args[0].toString(true)+
                  this.head.name+
                  this.args[1].toString(true)
        i=2
        if(this.args.length==2) return p?"("+result+")":result
        else result="("+result+")"
      }
      else{
        result = this.head.toString(p)
      }
      while(i<this.args.length){
        result+=" "+this.args[i].toString(true)
        i++
      }
      return p?"("+result+")":result
    }
  }
}
class Variable{
  constructor(owner,argIndex){
    this.owner=owner
    this.argIndex=argIndex
    this.owner.argCounts[argIndex]+=1
  }
  reduce(){return this}
  toString(p){
    return makeVarname(this.owner.lamIndex,this.argIndex)
  }
}

function buildTerm(g){
  let term={}
  g.term = term
  if (g.isHole){
    term.__proto__=Lambda.prototype
    term.init(g.numOwned)
    term.argCounts=(new Array(term.numArgs)).fill(0)
    if (g.filled){
      term.body=buildTerm(g.filled)
      /*for(let ch of g.children){
        if(ch!==g.filled){
          if(term.comment===undefined){
            term.comment= new Application(buildTerm(ch))
          }
          else{
            term.comment.args.push(buildTerm(ch))
          }
        }
      }*/
    }
    else{
      term.body=new Application(new Constant("_"))
      for(let ch of g.children){
        term.body.args.push(buildTerm(ch))
      }
    }
  }
  else{
    term.__proto__=Application.prototype
    term.args=[]
    if(g.owner!==root){
      term.head=new Variable(g.owner.term,g.ownerIndex)
    }
    for(let node of g.childNodes){
      if(node.nodeType==Node.TEXT_NODE){
        if (term.head) throw "More than 1 text node";
        term.head=new Constant(node.textContent)
      }
      else{
        term.args.push(buildTerm(node))
      }
    }
  }
  delete g.term //the children are constructed, so clean up
  return term
}
