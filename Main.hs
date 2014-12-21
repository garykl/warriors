module Main where


main :: IO ()
main = do
    putStrLn "Warriors"
    putStrLn "--------------------"


data Soul = Soul { initiative :: Double,
                   velocity :: Double,
                   vitality :: Double }

data Agent = Agent { x :: Double,
                     y :: Double,
                     lifepoints :: Double,
                     melee :: Double }

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


ki :: [Environment] -> [Action]
ki = undefined

type Team = [Warrior]

performAction :: Team -> Team -> (Team, Team)
performAction = undefined
