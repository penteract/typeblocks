module Sample where

f :: Int -> Int
f x = f x
g :: (String -> Int) -> String -> Int
g x y = x y
h :: (String -> Int) -> (Bool->String) -> Bool -> Int
h x y z = x (y z)

h' :: (String -> Int) -> (Bool->String) -> Bool -> Int
h' = \ x y z -> x (y z)

hh :: (String -> Int) -> (Bool->String) -> Bool -> Int
hh x = \ y z -> x (y z)


{-
tricky :: ((((Int->Int->Bool)->String)->Bool)->Char) ->  (((Int->Int->Bool)->String)->Bool) -> Char
tricky fn inner = fn (\ fn0 -> inner (\ inner0 -> fn0 (\ fn1v0 fn1v1 -> inner0 fn1v1 fn1v0)))
-}

{-
tricky :: ((((o->o->b)->s)->b)->r) ->  (((o->o->b)->s)->b) -> r

fn (\ fn0 -> _ (fn0 (\ fn1v0 fn1v1 -> inner (\ inner0 -> _ (inner0 fn1v1 fn1v0)))))

g :: (String -> Int) -> String -> Int
g x y = x y

g2 :: (String -> Int) -> String -> Int
g2 x = x

g3 :: (String -> Int) -> String -> Int
g3 x = (\ y -> x y)

h :: Int -> Int
h x = f (x+x)


("h'",Defn ([],Nothing) [Line (["|->"],Nothing) (Operator (["h'"],Nothing) [Var ([],Just 0) [LHSAntiHole ([],Just 0) []],Var ([],Just 0) [LHSAntiHole ([],Just 0) []],Var ([],Just 0) []])
(Filled ([],Nothing)
  (Symbol ([],Just 0) [Filled ([],Just 1)
    (Symbol ([],Just 1) [Filled ([],Just 1)
      (Symbol ([],Just 1) []) []]) []]) [])])

("f",Defn ([],Nothing) [Line (["|->"],Nothing) (Operator (["f"],Nothing) [Var (["x"],Just 0) []]) (Filled ([],Nothing) (Symbol (["f"],Just 0) [Filled ([],Just 1) (Symbol (["x"],Just 1) []) []]) [])])

("g",Defn ([],Nothing) [Line (["|->"],Nothing) (Operator (["g"],Nothing) [Var (["x"],Just 0) [LHSAntiHole ([],Just 0) []],Var (["y"],Just 0) []]) (Filled ([],Nothing) (Symbol (["x"],Just 0) [Filled ([],Just 1) (Symbol (["y"],Just 1) []) []]) [])])

("h",Defn ([],Nothing) [Line (["|->"],Nothing) (Operator (["h"],Nothing) [Var (["x"],Just 0) [LHSAntiHole ([],Just 0) []],Var (["y"],Just 0) [LHSAntiHole ([],Just 0) []],Var (["z"],Just 0) []])
(Filled ([],Nothing)
  (Symbol (["x"],Just 0) [Filled ([],Just 1)
    (Symbol (["y"],Just 1) [Filled ([],Just 1)
      (Symbol (["z"],Just 1) []) []]) []]) [])])

("hh",Defn ([],Nothing) [Line (["|->"],Nothing) (Operator (["hh"],Nothing) [Var (["x"],Just 0) [LHSAntiHole ([],Just 0) []],Var ([],Just 0) [LHSAntiHole ([],Just 0) []],Var ([],Just 0) []])
(Filled ([],Nothing)
  (Symbol (["x"],Just 0) [Filled ([],Just 1)
    (Symbol ([],Just 1) [Filled ([],Just 1)
      (Symbol ([],Just 1) []) []]) []]) [])])



-}
