module Warrior where

import Geometry


-- | represent different classes of warriors. Mainly interesting for drawing,
-- but there are possible use cases for the logic, like usage of certain
-- actions, specific to the class.
data WarriorClass = MeleeWarrior
                  | Wobble
                    deriving (Enum, Bounded, Show, Eq, Ord)

-- | the soul consists of properties, that usually are not changeable
data Soul = Soul { figureClass :: WarriorClass,
                   size :: Float,      -- ^ size is relevant for movement
                   velocity :: Float,  -- ^ potential maximum velocity
                   vitality :: Float,  -- ^ influences @Agent@s lifepoints
                   strength :: Float } -- ^ influences @Agent@s melee
                       deriving Show

meleeDuration :: Int
meleeDuration = 15

meleeHitTime :: Int
meleeHitTime = 5

meleeDistance :: Float
meleeDistance = 110

meleeDamage :: Float
meleeDamage = 1


-- | @ActionStatus@ is either
-- Moving or
-- MeleeData: Amount, Phase, Target relative to attackers position
data ActionStatus = Faineancing | Moving Vector | MeleeAttacking Float Int Vector
                        deriving Show


-- | the @Agent@ is the suffering part during the simulation. Values may
-- constantly change, due to attack, spells or movements.
data Agent = Agent { position :: Position,
                     lifepoints :: Float,
                     actionStatus :: ActionStatus } deriving Show


-- | the @Warrior@ is a virtual robot in arbitrary form, with an associating
-- artificial intelligence.
data Warrior = Warrior Soul Agent deriving Show


agentDead :: Agent -> Bool
agentDead agent = lifepoints agent <= 0


warriorDead :: Warrior -> Bool
warriorDead = liftWarrior agentDead


agentDistance :: Agent -> Agent -> Float
agentDistance a1 a2 = vectorLength $ position a1 .-. position a2


warriorCross :: Warrior -> Warrior -> Bool
warriorCross (Warrior sl1 ag1) (Warrior sl2 ag2) =
    agentDistance ag1 ag2 < size sl1 + size sl2


modifyAgent :: (Agent -> Agent) -> Warrior -> Warrior
modifyAgent f (Warrior soul agent) = Warrior soul (f agent)


modifySoul :: (Soul -> Soul) -> Warrior -> Warrior
modifySoul f (Warrior soul agent) = Warrior (f soul) agent


liftWarrior :: (Agent -> a) -> Warrior -> a
liftWarrior f (Warrior _ agent) = f agent


liftWarriors :: (Agent -> Agent -> a) -> Warrior -> Warrior -> a
liftWarriors f (Warrior _ a1) (Warrior _ a2) = f a1 a2


--------------------------------------------------------------------------------
-- warrior constructors

meleeWarrior :: Position -> Warrior
meleeWarrior pos = Warrior
    Soul { figureClass = Wobble,
           size = 40,
           velocity = 1,
           vitality = 1,
           strength = 1 }
    Agent { position = pos,
            lifepoints = 10,
            actionStatus = Faineancing }
