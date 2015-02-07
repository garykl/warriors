module Debug where


import System.IO.Unsafe (unsafePerformIO)


debug :: Show a =>  a -> a
debug a = unsafePerformIO $ print a >> return a
