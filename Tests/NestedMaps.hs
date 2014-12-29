module Tests.NestedMaps where

import Test.QuickCheck
import qualified NestedMap as N


main :: IO ()
main = do
    check "really empty:         " reallyEmpty
    check "really singleton:     " reallySingleton
    check "really added:         " reallyAdded
    check "really dont duplicate:" reallyDontDuplicate
    check "really delete:        " reallyDelete

  where
    check :: (Arbitrary a, Show a) => String -> (a -> Bool) -> IO ()
    check s f = putStr (s ++ " ") >> quickCheck f



-- | an empty map has no elements
reallyEmpty :: (Int, Int, Int) -> Bool
reallyEmpty (a, b, c) = null $ N.elemsDeep (N.empty :: N.Nmap Int Int Int)

-- | a singleton map has one element
reallySingleton :: (Int, Int, Int) -> Bool
reallySingleton (a, b, c) =
    let s = N.singleton a b c
    in  length (N.elemsDeep s) == 1 && length (N.elems s) == 1

-- | a nested map contains the element added
reallyAdded :: (Int, Int, Int) -> Bool
reallyAdded (a, b, c) = (c N.->- (a, b) $ N.empty) N.! (a, b) == c

-- | adding an element twice is like adding it once
reallyDontDuplicate :: (Int, Int, Int) -> Bool
reallyDontDuplicate (a, b, c) =
    let nmap = N.singleton a b c
    in  (length . N.elemsDeep $ nmap) ==
        (length . N.elemsDeep $ (a, b) N.-<- c $ nmap)

-- | delete from a singleton!
reallyDelete :: (Int, Int, Int) -> Bool
reallyDelete (a, b, c) = null . N.elems . N.delete (a, b) $ N.singleton a b c
