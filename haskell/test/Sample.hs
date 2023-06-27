module Sample where

f :: Int -> Int
f x = f x
{-
g :: (String -> Int) -> String -> Int
g x y = x y

g2 :: (String -> Int) -> String -> Int
g2 x = x

g3 :: (String -> Int) -> String -> Int
g3 x = (\ y -> x y)

h :: Int -> Int
h x = f (x+x)
-}
