module Main where

import Logic
import Draw.Draw (mainLoop)

garyKi :: Intelligence
garyKi _ _ = Melee (Pos 0 0)
mmpiKi :: Intelligence
mmpiKi _ _ = MoveTo (Pos 100 100)

main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField hhh -- (performActions garyKi mmpiKi)

