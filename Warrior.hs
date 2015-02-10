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

meleeDuration :: Soul -> Int
meleeDuration soul = round $ velocity soul + strength soul

-- TODO: Matthias, what is this? Removing from Logic?
meleeHitTime :: Int
meleeHitTime = 5

meleeDistance :: Soul -> Float
meleeDistance soul = 2 * size soul

meleeDamage :: Soul -> Float
meleeDamage = strength


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
warriorDead = liftAgent agentDead


agentDistance :: Agent -> Agent -> Float
agentDistance a1 a2 = distanceToAgent a1 $ position a2


agentToAgentVector :: Agent -> Agent -> Vector
agentToAgentVector a1 a2 = position a1 .-. position a2


distanceToAgent :: Agent -> Position -> Float
distanceToAgent a pos = vectorLength $ position a .-. pos


warriorCross :: Warrior -> Warrior -> Bool
warriorCross (Warrior sl1 ag1) (Warrior sl2 ag2) =
    agentDistance ag1 ag2 < size sl1 + size sl2


-- | @beatable w1 w2@ checks if @Warrior@ w1 can hit w2 by Melee
beatable :: Warrior -> Vector -> Warrior -> Bool
beatable w1 vec w2 =
    let target = beatCenter w1 vec
    in  vectorLength (target .-. liftAgent position w2) <= liftSoul size w2


-- | calculate the position at which a warrior would hit in a certain direction
beatCenter :: Warrior -> Vector -> Position
beatCenter w v =
    liftAgent position w .+ (liftSoul meleeDistance w |*| normalize v)


modifyAgent :: (Agent -> Agent) -> Warrior -> Warrior
modifyAgent f (Warrior soul agent) = Warrior soul (f agent)


modifySoul :: (Soul -> Soul) -> Warrior -> Warrior
modifySoul f (Warrior soul agent) = Warrior (f soul) agent


liftAgent :: (Agent -> a) -> Warrior -> a
liftAgent f (Warrior _ agent) = f agent


liftSoul :: (Soul -> a) -> Warrior -> a
liftSoul f (Warrior soul _) = f soul


liftAgents :: (Agent -> Agent -> a) -> Warrior -> Warrior -> a
liftAgents f (Warrior _ a1) (Warrior _ a2) = f a1 a2


--------------------------------------------------------------------------------
-- warrior constructors

meleeWarrior :: Position -> Warrior
meleeWarrior pos = Warrior
    Soul { figureClass = MeleeWarrior,
           size = 50,
           velocity = 5,
           vitality = 10,
           strength = 10 }
    Agent { position = pos,
            lifepoints = 30,
            actionStatus = Faineancing }
