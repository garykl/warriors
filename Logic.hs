module Logic(performAction,FieldState,Warrior) where

import qualified Data.Map as M


data WarriorClass = MeleeWarrior
--                    | Wizard
                    deriving (Enum,Bounded,Show)

data Soul = Soul { figureClass :: WarriorClass,
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
                              lifepoints = lifepoints agent + lifepoints effect
--                              melee = melee agent + melee effect
                              }

data Warrior = Warrior Soul Agent
type BluredWarrior = Warrior


-- | each Warrior sees its own environment.
data Environment = Env Warrior [Warrior] [BluredWarrior]

data Action = Melee Double Double
            | MoveTo Double Double

ki :: [Environment] -> [Action]
ki = undefined

type Team = M.Map String Warrior
type FieldState = (Team, Team)

performAction :: Team -> Team -> FieldState
performAction = undefined
