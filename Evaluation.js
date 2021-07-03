"use strict";

function singleStep(g){
  console.log(g)
  let defn = g.defn
  if(!defn){
    for (let hole of g.children){
      if (hole.filled){
        if (singleStep(hole.filled)) return true
      }
      else for (let ch of hole.children){
        if (singleStep(ch)) return true
      }
    }
    return false;
  }
  console.log("looking through lines")
  for(let line of defn.children){
    console.log(line)
    if(matches(line.lhs,g)){
      subst(line.rhs, g)
      return true
    }
  }
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
// substitues the contents of a hole in place of a term
function subst(hole,term){
  console.log("starting subst")
  let h = hole.duplicate() // really, we should do stuff during duplicate, but this is easier for now
  h.mapsto = term
  replaceWithin(h)
  let filling
  let newHole = term.parentElement
  if(filling=h.filled){
    detach(filling)
    newHole.insertBefore(filling,term)
    if(isArg(term)){
      newHole.classList.add("filled")
      newHole.filled=filling
      filling.classList.add("filling")
    }
    filling.setxy(term.x,term.y)
  }
  else{
    for(let ch of h.children){
      detach(ch)
      newHole.insertBefore(ch,term)
    }
  }
  deleteAll(term)
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
