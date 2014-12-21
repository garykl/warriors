module Main where

import Logic (initialField, performAction)
import Draw.Draw (mainLoop)


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop initialField performAction

