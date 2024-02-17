// Data structure of an expression
// data Term = App Term Term | Const String | Variable String | AppL Term [String] Term

//data Type = Base String           | Poly String               | Arrow Type Type
//            {name:"Type",args:[]} | {name:"type",args:[]}      | {name:"->", args:[Type,Type]}


// definition boxes and non-lhs named boxes have a typeScope property.
// This is an object with names of type variables as keys
// The values associated with each key are [S,ufds] - these length-2 arrays are used as ufds links
// Where S is the set of all boxes whose outer type involves that type variable
// and ufds is the link in a union-find-disjoint-sets datastructure, linking to another type variable, itself, or a concrete type
// Whenever a link is broken, the ufds components each side of it need to be recomputed, although this can be short-circuited
//    if a path from one side to the other is found.

function isBase(t){
  let c = t.name[0]
  return t.args.length==0 && c!==c.toLowerCase()
}
function isTyVar(t){//Return true if the type is headed by a type variable
  let c = t.name[0]
  return c!==c.toUpperCase()
}
function isFn(t){
  return t.name==="->"
}
function showType(t){
  return isFn(t)?`(${showType(t[0])})->(${showType(t[1])})`:t.name
}

function extractFn(t){
  if(!isFn(t))throw "Not a function type"
  return t.args
}
function* extractVars(t){
  let c = t.name[0]
  if (c!==c.toUpperCase()){yield t.name}
  for(let arg of t.args) {for(let x in extractVars(arg)) {yield x}}
}
function mkBoxType(t,box){
  //Turn a parsed type into a value suitable for boxType
  let vs = box.typeScope.tyVars
  let tvar
  let newTy = {name:t.name,args:[]}
  if (isTyVar(t)){
    let k = t.name
    if(k in vs){
      tvar = vs[k]
      tvar[0].add(box)
    } else{
      let s = new Set()
      tvar=[s,undefined]
      tvar[1]=tvar
      vs[k]=tvar
    }
    newTy.var=tvar
  }
  for (let arg of t.args){
    newTy.args.append(mkBoxType(arg,box))
  }
  return newTy
}

// Given a graph with nodes of the shape ["a",scope] and undirected edges represented by non-erroring
// forall x in scope.tyVars["a"]:  [x.filled.boxType.name,x.filled.typeScope]
// forall x in scope.tyVars["a"]: if x.parentElement.filled===x: [x.parentElement.boxType.name,x.parentElement.typeScope]
// find the connected components of that graph
function tyEq(t1,t2){
  if(isTyVar(t1)!==isTyVar(t2)) return false
  if(isTyVar(t1)){
    if (t1.var!==t2.var) return false
  }else{
    if (t1.name!==t2.name) return false
  }
  if (ty1.args.length!==ty2.args.length) return false
  for(let i = 0;i<ty1.args.length;i++){
    if(!tyEq(ty1.args[i],ty2.args[i])) return false
  }
  return true
}
function unify(){
}