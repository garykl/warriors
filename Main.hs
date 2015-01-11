module Main where

import Logic
import Draw.Draw (mainLoop)

import Geometry
import qualified NestedMap as N


garyKi :: Intelligence
garyKi _ = MoveTo (Pos 100 300)

gameKi :: Intelligences
gameKi = N.singleton "Holzfaeller" "Heinz" garyKi

initialWarrior :: Warrior
initialWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                           (Agent (Pos 200 50) 1 (MeleeAttacking 0 0 (Vec (0-100) (0+10))))
--                          (Agent (Pos 0 0) 1 (MeleeAttacking 0 0 (Vec (0-100) (0-70))))
--                          (Agent (Pos 0 0) 1 (MeleeAttacking 0 0 (Vec (50) (10))))
--                          (Agent (Pos 0 0) 1 (MeleeAttacking 0 0 (Vec (50) (0-70))))

initialField :: Field
initialField = N.singleton "Holzfaeller" "Heinz" initialWarrior


hhh :: Field -> Field
hhh field =
    let Warrior soul agent = field N.! ("Holzfaeller", "Heinz")
        MeleeAttacking amount phase position = actionStatus agent
    in  N.singleton "Holzfaeller" "Heinz"
            $ Warrior soul
                    $ agent {actionStatus =
                        MeleeAttacking amount
                                       ((phase + 1) `mod` meleeDuration)
                                       position }


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField $ performTimestep gameKi -- hhh

