module Tests.Animate where

import Game
import Warrior
import Logic
import Draw.Draw (mainLoop)

import Geometry
import qualified NestedMap as N
import qualified Data.Map as M

import qualified Tribes.Garyki as Gary
import qualified Tribes.Mmpiki as Mmpi
import qualified Tribes.Moving5 as Mv5


helper :: ((Warrior, Intelligence) -> c) -> N.Nmap TribeName WarriorName c
helper f =
    composeAll
      -- [("Holzfaeller", name) N.-<- f value |
      --     (name, value) <- zip (M.keys Mv5.provide) (M.elems Mv5.provide)]
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
    let field = loopIf (not . fightDecided)
                       1000
                       (performTimestep gameKi)
                       initialField
    print field


loop :: Int -> (a -> a) -> a -> a
loop n f = composeAll (replicate n f)


loopIf :: (a -> Bool) -> Int -> (a -> a) -> a -> a
loopIf condition n f =
    let modF b = if condition b then f b else b
    in  loop n modF
