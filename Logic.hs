module Logic where

import qualified Data.Map as M


-- do not make them Num, because of abs, signum and fromInteger
data Position = Pos Float Float deriving Show
data Vector = Vec Float Float

class Addable a where
    (|+|) :: a -> a -> a
    (|-|) :: a -> a -> a

instance Addable Position where
    Pos x1 y1 |+| Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
    Pos x1 y1 |-| Pos x2 y2 = Pos (x1 - x2) (y1 - y2)

instance Addable MeleeData where
    MeleeData a1 p1 pos1 |+| MeleeData a2 p2 pos2 =
        MeleeData (a1 + a2) (p1 + p2) (pos1 |+| pos2)
    MeleeData a1 p1 pos1 |-| MeleeData a2 p2 pos2 =
        MeleeData (a1 - a2) (p1 - p2) (pos1 |-| pos2)

-- instance Addable Agent where
--     Agent 


-- | represent different classes of warriors. Mainly interesting for drawing,
-- but there are possible use cases for the logic, like usage of certain
-- actions, specific to the class.
data WarriorClass = MeleeWarrior
--                    | Wizard
                    deriving (Enum, Bounded, Show, Eq, Ord)

-- | the soul consists of properties, that usually are not changeable
data Soul = Soul { figureClass :: WarriorClass,
                   -- initiative :: Rational,
                   velocity :: Float,  -- ^ potential maximum velocity
                   vitality :: Float,  -- ^ influences @Agent@s lifepoints
                   strength :: Float } -- ^ influences @Agent@s melee

meleeDuration :: Int
meleeDuration = 15

meleeHitTime :: Int
meleeHitTime = 10

-- | @Melee@ = (Amount, Phase, Target/Position)
data MeleeData = MeleeData Float Int Position

-- | the @Agent@ is the suffering part during the simulation. Values may
-- constantly change, due to attack, spells or movements.
data Agent = Agent { position :: Position,
                     lifepoints :: Float,
                     melee :: MeleeData }

-- | when performing an @Action@, an @Effect@ is generated, that is applied to
-- the warriors @Agent@.
type Effect = Agent

-- | an @Agent@ is changed by an @Effect@ by adding certain fields.
instance Addable Agent where
    agent |+| effect = agent { position = position agent |+| position effect,
                               lifepoints = lifepoints agent + lifepoints effect,
                               melee = melee agent |+| melee effect }
    agent |-| effect = agent { position = position agent |-| position effect,
                               lifepoints = lifepoints agent - lifepoints effect,
                               melee = melee agent |-| melee effect }

-- | the @Warrior@ is a virtual robot in arbitrary form, with an associating
-- artificial interlligence.
data Warrior = Warrior Soul Agent

-- | the @Tribe@ is a collection of @Warrior@s. They are thought to be fighting
-- against another @Tribe@. Additionally, the @Tribe@ gives each Warrior its
-- identity.
type Tribe = M.Map String Warrior

-- | each @Warrior@ sees its own environment, that consists of its own @Team@
-- and the enemy team.
data Environment = Env Tribe Tribe

-- | @Intelligence@ is, when you are able to conclude different actions for
-- different @Environment@s.
type Intelligence = String -> Environment -> Action


-- | @Warrior@s interact with each other and with themselves via @Action@s.
-- @Action@s are converted into @Effect@s, which are then added to the
-- @Agent@s.
data Action = Melee Position
            | MoveTo Position


-- | the @Field@ is given by considering two Teams.
type Field = (Tribe, Tribe)


-- | a @Warrior@ on the @Field@ can be identified by its name (the key of the
-- @Tribe@ @M.Map@) and the @Tribe@ number (tribe == fst field <=> 1, ...).
type WarriorIdentifier = (Int, String)


initialField :: Field
initialField = (M.singleton "Heinz" $
    Warrior (Soul MeleeWarrior 1 1 1)
            (Agent (Pos 0 0) 1 (MeleeData 0 0 (Pos 0 0))), M.empty)


hhh :: Field -> Field
hhh (tribe, _) =
    let Warrior soul agent = tribe M.! "Heinz"
        MeleeData amount phase position = melee agent
    in  (M.singleton "Heinz" $ Warrior soul $
            agent {melee = MeleeData amount
                                     ((phase + 1) `mod` meleeDuration)
                                     position },
         M.empty)


-- | each @Warrior@ on the @Field@ can try to act appropriately.
performActions :: Intelligence -> Intelligence -> Field -> Field
performActions i1 i2 f@(t1, t2) =
    let order = getOrder t1 t2  -- TODO: at the moment, its in sequence, later,
                                --       there will be the initiative criterion.
    in  f


performAction :: String -> Intelligence -> Field -> Field
performAction name intelligence field =
    let environment = getEnvironment field
        action = intelligence name environment
    in  field


-- | create the @Environment@ of a @ThinkingWarrior@. This should depend on the
-- @Agent@, which dicides how well it is recognized.
-- TODO: implement the idea. current state: everything is recognized as it is.
getEnvironment :: Field -> Environment
getEnvironment (t1, t2) = Env t1 t2


-- | extract a @ThinkingWarrior@ out of the @Field@
getWarrior :: WarriorIdentifier -> Field -> Warrior
getWarrior (belonging, name) (t1, t2) =
    case belonging of
        1 -> t1 M.! name
        2 -> t2 M.! name

-- | @Warrior@s may be faster of slower.
-- TODO: implement this idea. current status: random order of @Tribe@ 1 followed
-- by random order of @Tribe@ 2.
getOrder :: Tribe -> Tribe -> [WarriorIdentifier]
getOrder t1 t2 = zipWithNumber 1 (M.keys t1) ++ zipWithNumber 2 (M.keys t2)
  where zipWithNumber :: Int -> [a] -> [(Int, a)]
        zipWithNumber n ll = zip (replicate (length ll) n) ll
