module Main where

import Logic (performAction)
import Draw.Draw (mainLoop)


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"
    mainLoop performAction

