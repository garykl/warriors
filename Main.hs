module Main where

import Logic
import Draw.Draw (mainLoop)

main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField hhh -- (performActions garyKi mmpiKi)

