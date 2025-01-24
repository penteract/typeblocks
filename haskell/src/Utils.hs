module Utils where
import System.IO.Unsafe

sup s = seq (unsafePerformIO (print s))
up x = sup x x

hashStr :: String -> Int
hashStr [] = 13
hashStr (c:cs) = (fromEnum c)+ 37*(hashStr cs)
