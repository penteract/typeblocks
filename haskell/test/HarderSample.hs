module HarderSample where

-- some things I didn't know were legal syntax

(+-+) :: Int -> Bool -> Int
x +-+ True = x+1
(+-+) x False = x

zero,one :: Int
zero = 0
one = 1
