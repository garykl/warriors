module Main where

import qualified Data.Map as M


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"


data Class = MeleeWarrior | Wizard

data Soul = Soul { figureClass :: Class,
                   -- initiative :: Rational,
                   velocity :: Double,
                   vitality :: Double,
                   strength :: Double }

data Agent = Agent { x :: Double,
                     y :: Double,
                     lifepoints :: Double,
                     -- melee :: Double,
                     meleePhase :: Double }
type Effect = Agent

affect :: Agent -> Effect -> Agent
affect agent effect = agent { x = x agent + x effect,
                              y = y agent + y effect,
                              lifepoints = lifepoints agent + lifepoints effect,
                              melee = melee agent + melee effect }

data Warrior = Warrior Soul Agent
type BluredWarrior = Warrior


drawWarrior :: Warrior -> Picture
drawWarrior _ = undefined


-- | each Warrior sees its own environment.
data Environment = Env Warrior [Warrior] [BluredWarrior]

data Action = Melee Double Double
            | MoveTo Double Double
            | Follow Warrior


ki :: [Environment] -> [Action]
ki = undefined

type Team = M.Map String Warrior

performAction :: Team -> Team -> (Team, Team)
performAction = undefined
