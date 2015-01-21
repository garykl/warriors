module Rand where

import Geometry

import System.Random (randomIO)
import System.IO.Unsafe (unsafePerformIO)


-- | produce a random number from 0 to n
randomTo :: Int -> Int
randomTo n = unsafePerformIO randomIO `mod` n


-- | random number from n to m
randomFromTo :: Int -> Int -> Int
randomFromTo n m = n + randomTo m


-- | random float between low and high
-- this function needs argument, for returning random number. An implementation
-- without argument was memoizing the first used random number.
random :: Float -> Float -> Float
random low high = low + (high - low) * fromIntegral (randomTo 100) / 100


-- | blur a data type
class Randomizable a where
    -- | the first argument is the blurring amount, the second one is to be
    -- blurred.
    randomize :: a -> a -> a

instance Randomizable Position where
    randomize (Pos rx ry) (Pos x y) =
        Pos (random (x - rx) (x + rx))
            (random (y - ry) (y + ry))


