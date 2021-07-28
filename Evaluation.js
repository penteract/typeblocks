"use strict";
//Single step evaluation : resembles monadic bind
// return false to signal that a reduction has occurred
let retType = undefined // for ret
HTMLDivElement.prototype.evl = function(callback){
  if(this.isHole){
    throw "shouldn't try to evaluate a hole (probably unfilled hole being passed to builtin)"
  }
  let defn = this.defn
  if(!defn){
    if (this.isConstructor){
      return callback(eval(this.text))
    }
    for (let hole of this.children){
      if (hole.filled){
        if (!hole.filled.evl((x)=>true)) return false;
      }
      else for (let ch of hole.children){
        if (!ch.evl((x)=>true)) return false
      }
    }
    return true;
  }
  if(defn.__proto__===HTMLDivElement.prototype){
    console.log("looking through lines")
    for(let line of defn.children){
      console.log(line)
      if(matches(line.lhs,this)){
        subst(line.rhs, this)
        return false
      }
    }
  }
  else{//builtin
    let args = []
    for(let hole of this.children){
      args.push(hole.filled?hole.filled:hole)
    }
    let savedRetType = retType
    retType = this.baseType
    let newTerm = defn(...args)
    retType = savedRetType
    if(newTerm){
      replace(this, newTerm)
    }
    return false
  }
}
function ret(x){
  if (retType===undefined){
    throw "bad call to ret"
  }
  let g = makeBox(x+"",retType)
  g.isConstructor=true
  return g
}
function matches(lhs, t){
  return true
  /*
  let formals = lhs.children
  let actuals = t.children
  if(formals.length!=actuals.length){
    console.log(lhs,t)
    throw "Mismatched formal and actual parameters"
  }
  for(let i=0;i<formals.length;i++){
    if(!actuals[i].filled) return false
    //formals[i].subst=actuals[i]
  }*/
}
//replace a term by another
function replace(term, newTerm){
  let newHole = term.parentElement
  detach(newTerm)
  newHole.insertBefore(newTerm,term)
  if(isArg(term)){
    newHole.classList.add("filled")
    newHole.filled=newTerm
    newTerm.classList.add("filling")
  }
  newTerm.setxy(term.x,term.y)
  deleteAll(term)
}
// substitues the contents of a hole in place of a term
function subst(hole,term){
  console.log("starting subst")
  let h = hole.duplicate() // really, we should do stuff during duplicate, but this is easier for now
  h.mapsto = term
  replaceWithin(h)
  let filling
  let newHole = term.parentElement
  if(filling=h.filled){
    replace(term,filling)
  }
  else{
    for(let ch of h.children){
      detach(ch)
      newHole.insertBefore(ch,term)
    }
    deleteAll(term)
  }
  detach(h)
  delete h.mapsto
}

// replace all occurences of variables for which the scope has 'mapsto' defined
function replaceWithin(g,allch){
  if(g.isHole){
    if(!allch && g.filled){
      replaceWithin(g.filled)
      return;
    }
  }
  else if (g.scope.mapsto){
    subst(g.scope.mapsto.children[g.scopeIndex], g)
    return;
  }
  let chs = []
  for (let ch of g.children) (chs.push(ch)) // we might mutate g.children
  for (let ch of chs) {
    replaceWithin(ch,true)
  }
}

function deleteAll(x){
  detach(x)
  x.remove()
  // try the following if this stops working?
  /*x.visit((n)=>{
    detach(n)
    n.remove()
  })*/
}
