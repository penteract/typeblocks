
function assertEq(a,b){
  if (a!==b && JSON.stringify(a)!==JSON.stringify(b)) throw ("not Equal: "+a+" and "+b)
}

// Data structure of an expression
// data Term = App Term Term | Const String | Variable String | AppL Term [String] Term

//data Type = Base String | Arrow Type Type
//            "Type"      | [Type,Type]

Types = {} // Set of base types
Constants = {} //dictionary giving Types

function addType(t){
  Types[t]=true
}
function addConst(c,t){
  Constants[c]=t
}
function isBase(t){
  return typeof(t)=="string"
}

token = /(\p{Z}+)|(\()|(\))|((?:(?![\(\)])(?:\p{M}|\p{S}|\p{P}))+)|((?:\p{L}|\p{N})+)/u
SPACE=1
OPENPAREN=2
CLOSEPAREN=3
SYMBOL=4
WORD=5

function isInfix(s){
  return s.match(/^(?:\p{M}|\p{S}|\p{P})+$/u)
}

function partition(s){
  // Split a string into parenthesised parts, and by spaces in unparenthesised parts recursively
  var seen=[]
  while (s.length>0){
    var match=s.match(token)
    if (match===null) throw ("unable to match from "+s)
    var tok = match[0]
    s=s.slice(tok.length)
    if (match[SPACE]){
    }
    else if(match[OPENPAREN]){
      [part,s]=partition(s)
      seen.push(part)
    }
    else if (match[CLOSEPAREN]){
      return [seen,s]
    }
    else if (match[SYMBOL] || match[WORD]){
      seen.push(tok)
    }
    else throw "I've probably forgotten to adjust constants"
  }
  return [seen,s]
}

function parseLine(line){
  if (line.startsWith("data "))
    return parseData(line.slice(5).trim())
}
function parseData(data){
  d = data.split("=")
  if(d.length!=2) throw ("expected something of the form a = b\nreceived: "+data);
  [head,tail] = d
  head = head.trim()
  addType(head)
  tail=tail.trim()
  if (tail==="")return// Void
  for(opt of tail.split("|")){
    thing = partition(opt)[0]
    if (thing.length<1) throw ("empty component in datatype declaration: "+tail)
    constructor=thing[0]
    typ=thing.slice(1).flatMap(x=>[x,"->"])
    typ.push(head)
    addConst(thing[0],parseType(typ))
  }
}
function parseType(type){
  //Parse the result of partition as a type
  if (type.length<1) throw ("empty component type in "+type)
  var l=type.length-1
  var part=type[l]
  if (part==="->" || part==="→") throw "unexpected arrow in "+type
  var rhs=typeof(part)==="string"?part:parseType(part)
  while(l>0){
    l-=1; part=type[l]
    if (! (part==="->" || part==="→")) throw "expected arrow in "+type+"\npart: "+part
    if (l==0)throw "expected term to left of arrow in "+ type
    l-=1; part=type[l]
    if (part==="->" || part==="→") throw "unexpected arrow in "+type
    var lhs=typeof(part)==="string"?part:parseType(part)
    rhs=[lhs,rhs]
  }
  return rhs
}
assertEq(parseType(partition("(a→d)->b->c")[0]), [["a","d"],["b","c"]])

function showType(t){
  return typeof(t)==="string"?t:`(${showType(t[0])})->(${showType(t[1])})`
}
