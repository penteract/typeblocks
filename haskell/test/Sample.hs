module Sample where

f :: Int -> Int
f x = f x
{-
g :: (String -> Int) -> String -> Int
g x y = x y
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
-}
