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

meleeDuration :: Int
meleeDuration = 15

meleeHitTime :: Int
meleeHitTime = 5

meleeDistance :: Float
meleeDistance = 30

meleeDamage :: Float
meleeDamage = 1


-- | @ActionStatus@ is either
-- Moving or
-- MeleeData: Amount, Phase, Target relative to attackers position
data ActionStatus = Faineancing | Moving | MeleeAttacking Float Int Vector


-- | the @Agent@ is the suffering part during the simulation. Values may
-- constantly change, due to attack, spells or movements.
data Agent = Agent { position :: Position,
                     lifepoints :: Float,
                     actionStatus :: ActionStatus }


-- | the @Warrior@ is a virtual robot in arbitrary form, with an associating
-- artificial intelligence.
data Warrior = Warrior Soul Agent
