"use strict";

"(+) :: Int -> Int -> Int"
function plus(a,b){
  return a.evl( (av) =>
         b.evl( (bv) =>
         ret(av+bv)
  ))
}

function liftA2(f){
  return ((a,b)=>
    a.evl( (av) =>
    b.evl( (bv) =>
    ret(f(av,bv))
    ))
  )
}
function fmap(f){
  return ((a)=>
    a.evl( (av) =>
    ret(f(av))
    )
  )
}

let builtins = {
  "div" : ["Int -> Int -> Int", liftA2((a,b)=>Math.floor(a/b))],
  "ifThenElse" : ["Bool -> Int -> Int -> Int",
  function (cond,then,els){
    return cond.evl((c)=>
      c?then:els
    );
  }],
  "negate" : ["Int -> Int", fmap((x)=>-x)]
}
for(let c of ["+","-","*"]){
  builtins[c] = ["Int -> Int -> Int", liftA2(Function("return arguments[0]"+c+"arguments[1]"))]
}
for(let c of [">","<",">=","==","<="]){
  builtins[c] = ["Int -> Int -> Bool", liftA2(Function("return arguments[0]"+c+"arguments[1]"))]
}
