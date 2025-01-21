module Utils where
import System.IO.Unsafe

sup s = seq (unsafePerformIO (print s))
up x = sup x x
