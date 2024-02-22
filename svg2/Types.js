"use strict";
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

class TyVar{
  constructor(scope,name){
    scope.tyVars[name]=this
    this.uses=new Set()
    this.ufds=this
    this.scope=scope
    this.vname=name
  }
}

function isBase(t){
  let c = t.name[0]
  return t.args.length==0 && c!==c.toLowerCase()
}
function isPolyVar(t){//Return true if the type is headed by a type variable
  let c = t.name[0]
  return c!==c.toUpperCase()
}
function isFn(t){
  return t.name==="->"
}
function showType(t){
  return isFn(t)?`(${showType(t.args[0])})->(${showType(t.args[1])})`:t.name
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
  if (isPolyVar(t)){
    let k = t.name
    if(k in vs){
      tvar = vs[k]
    } else{
      tvar = new TyVar(box.typeScope,k)
    }
    tvar.uses.add(box)
    newTy.var=tvar
  }
  for (let arg of t.args){
    newTy.args.append(mkBoxType(arg,box))
  }
  return newTy
}

// Test whether a pair of types are identical, given for previous unification
function tyEq(t1,t2){
  t1=canonize(t1)
  t2=canonize(t2)
  if((t1 instanceof TyVar) !== (t2 instanceof TyVar)) return false
  if(t1 instanceof TyVar) return t1===t2
  if (t1.name!==t2.name) return false
  if (t1.args.length!==t2.args.length) return false
  for(let i = 0;i<t1.args.length;i++){
    if(!tyEq(t1.args[i],t2.args[i])) return false
  }
  return true
}
function getCanon(tv){
  let nx = tv.ufds 
  if(tv===nx)return tv;
  if(tv.ufds instanceof TyVar){
    let r = getCanon(nx)
    tv.ufds = r
    return r
  }
  else {return nx}
}
// I'm not doing just making this part of getCanon in the hope that it catches some type errors
function canonize(ty){ 
  if(ty instanceof TyVar) return getCanon(ty)
  if(isPolyVar(ty))return getCanon(ty.var)
  return ty
}

function tryToUnify(t1,t2){
  let ufds = new Map()
  function getU(x){
    x=canonize(x)
    if(!(x instanceof TyVar)) return x
    if(!ufds.has(x)){ufds.set(x,x)}
    let k = ufds.get(x)
    if(k!==x){
      let r=getU(k)
      ufds.set(x,r)
      return r
    }
    return x
  }
  function occurs(tv,ty){
    if(tv===ty) {return true}
    else if (!(ty instanceof TyVar)){
      for (let t of ty.args){
        t = getU(t)
        if(occurs(tv,t)){return true}
      }
    }
  }
  function unify(t1,t2){
    t1 = getU(t1)
    t2 = getU(t2)
    if(t1 instanceof TyVar){
      if(t1!==t2){
        if(occurs(t1,t2)) {console.log(t1,t2); return false}
        ufds.set(t1,t2)
      }
    }
    else if (t2 instanceof TyVar){
      if(occurs(t2,t1)) {console.log(t2,t1); return false}
      ufds.set(t2,t1)
    }
    else{
      if(t1.name!==t2.name) {return false}
      if(t1.args.length!==t2.args.length) {return false}
      for(let i=0; i<t1.args.length; i++){
        if(!unify(t2.args[i],t1.args[i])){return false}
      }
    }
    return true
  }
  if(unify(t1,t2)){
    return ufds
  }else{
    return false;
  }
}
function initTypeScope(scope){
  scope.tyVars={}
  scope.tmpCount=0//Numbr of temporary type variables that have ever been created
}
function freshTmpVar(scope){
  let t = "t-"+(scope.tmpCount++)
  let ty = {name:t,
    args:[],
    var: new TyVar(scope,t)
  }
  ty.var.tmp = true
  return ty
}

// Create a new argument box for each box bordered with a given type
// propagates through everything in the connected component
// visited is a set of type variables
// handled is a a map from boxes which have had an argument added to the subbox created for that box
function createArg(tv,visited,handled){
  let scope = tv.scope
  //let tv = scope.tyVars[varname]
  if(!visited){visited = new Set()}
  if(!handled){handled = new Map()}
  if(visited.has(tv))return;
  visited.add(tv)
  let freshVar = freshTmpVar(scope)
  for(let g of tv.uses){
    let conn = g.parentElement?.filled===g?g.parentElement:g.filled
    if(conn){
      if(!isPolyVar(conn.boxType)){
        throw "cannot connect"
      }
      createArg(conn.typeScope, conn.boxType.name, visited,handled)//TODO: consider polymorphic variables here
    }
    let ch = subBox("", freshVar, g.otherCols, g.cols, !g.isHole, g, g.isLHS, scope)
    if(g===g.parentElement?.lhs){makeDraggable(ch)}
    ch.scopeIndex = g.numOwned++
    handled.set(g,ch)
    dirty.push(ch)
    if(conn && handled.has(conn)){
      let otherCh = handled.get(conn)
      if(g.filled){//g is hole, ch is not hole
        dofill(ch,otherCh)
      }else{//ch is hole
        dofill(otherCh,ch)
      }
      getCanon(ch.boxType.var).ufds=getCanon(otherCh.boxType.var)
    }
    //TODO:
  }
  return freshVar
}
//visit each edge of a connected component in the concrete type unification graph
// callback is called on every occurence of each type variable in the component, 
// with the filling/filled paired element as the second argument if the box is filling/filled
function traverseComponent(tv, callback, visited){
  if(!visited){visited = new Set()}
  if(visited.has(tv))return;
  visited.add(tv)
  for(let g of tv.uses){
    let conn = g.parentElement?.filled===g?g.parentElement:g.filled
    let r
    if(r=callback(g,conn))return r
    if(conn && isPolyVar(conn.boxType)){
      if(r=traverseComponent(conn.boxType.var, callback, visited))return r
    }
  }
}

/*
Given a box, return the implicit type of of the concrete term it is part of, taking account of internal holes
*/
function buildType(g){
  let ty = g.boxType
  for(let arg of (g.isHole?[...g.boxes()]:getHoles(g)).reverse()){
    ty = {name:"->",args:[buildType(arg),ty]}
  }
  return ty
}
function printType(t){
  if(isFn(t)){
    return ("("+printType(t.args[0])+"->"+printType(t.args[1])+")")
  }
  else return t.name
}
// disconnections
function disconnectTypes(atype, htype){
  // We don't need to worry about these being arrow types
  let atv = atype.var
  let htv = htype.var
  // TODO(Type constructors): (if types get fancier, this check gets more complicated; possibly skip it altogether)
  if (!atv && !htv) return // If they aren't type variables, no need to worry 
  let [tv,ot] = atv?[atv,htype]:[htv,atype]
  let tvparts = new Set([tv])
  let nontvParts = new Set()
  if(!traverseComponent(tv, function(g,conn){
    if(conn){
      if(shallowEq(conn.boxType,ot))return true
      if(conn.boxType?.var)tvparts.add(conn.boxType.var)
      else nontvParts.add(conn.boxType)
    }
  })){
    let k
    for(let ty of nontvParts){
      k??=ty
      if (k.name!==ty.name) throw "Bad component"
    }
    k??=tv
    for(let v of tvparts){v.ufds=k}
    if(ot.var){
      tvparts = new Set([ot.var])
      nontvParts.clear()
      traverseComponent(tv, function(g,conn){
        if(conn){
          if(conn.boxType?.var)tvparts.add(conn.boxType.var)
          else nontvParts.add(conn.boxType)
        }
      })
      k=undefined
      for(let v of nontvParts){
        k??=v
        if (k.name!==v.name) throw "Bad component"
      }
      k??=ot.var
      for(let v of tvparts){v.ufds=k}
    }
  }
  // If the other type is in the component anyway, no action is necessary
}
// Determine if types are equal while the ufds graph may be invalid
function shallowEq(ty1,ty2){
  if(ty1.var){
    return ty1.var===ty2.var
  }
  if(ty2.var) return false
  if (ty1.name!==ty2.name) return false
  if (ty1.args.length!==ty2.args.length) return false
  for(let i = 0;i<ty1.args.length;i++){
    if(!shallowEq(ty1.args[i],ty2.args[i])) return false
  }
  return true
}

// see if a temporary type variable is still needed
function checkNeeded(box){

}