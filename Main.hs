module Main where

import Game
import Logic
import Draw.Draw (mainLoop)

import Geometry
import qualified NestedMap as N


garyKi :: Intelligence
garyKi _ = MoveTo (Pos 50 50)

mmpiKi :: Intelligence
mmpiKi _ = Melee (Vec 0 0)

gameKi :: Intelligences
gameKi = (("Holzfaeller", "Heinz") N.-<- mmpiKi)
       . (("Graeber", "Gerhardt") N.-<- garyKi)
       $ N.empty

mmpiWarrior :: Warrior
mmpiWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                      (Agent (Pos 50 50) 10 Moving)

garyWarrior :: Warrior
garyWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                      (Agent (Pos 400 400) 10 Moving)

initialField :: Field
initialField = (("Holzfaeller", "Heinz") N.-<- mmpiWarrior)
             . (("Graeber", "Gerhardt") N.-<- garyWarrior)
             $ N.empty



main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField $ performTimestep gameKi -- hhh

