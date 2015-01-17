module Main where

import Game
import Logic
import Draw.Draw (mainLoop)

import Geometry
import qualified NestedMap as N
import qualified Data.Map as M

import qualified Tribes.Garyki as Gary
import qualified Tribes.Mmpiki as Mmpi


helper :: ((Warrior, Intelligence) -> c) -> N.Nmap TribeName WarriorName c
helper f =
    composeAll
        ([("Holzfaeller", name) N.-<- f value |
            (name, value) <- zip (M.keys Mmpi.provide) (M.elems Mmpi.provide)]
      ++ [("Graeber", name) N.-<- f value |
            (name, value) <- zip (M.keys Gary.provide) (M.elems Gary.provide)])
       $ N.empty


gameKi :: Intelligences
gameKi = helper snd


initialField :: Field
initialField = helper fst


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField $ performTimestep gameKi -- hhh

