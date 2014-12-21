module Logic where

import qualified Data.Map as M


data Position = Pos Float Float
data Vector = Vec Float Float


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

-- | @Melee@ = (Amount, Phase, Target/Position)
type Melee = (Float, Float, Position)

-- | the @Agent@ is the suffering part during the simulation. Values may
-- constantly change, due to attack, spells or movements.
data Agent = Agent { position :: Position,
                     lifepoints :: Float,
                     melee :: Melee }

-- | when performing an @Action@, an @Effect@ is generated, that is applied to
-- the warriors @Agent@.
type Effect = Agent

-- | an @Agent@ is changed by an @Effect@ by adding certain fields.
affect :: Agent -> Effect -> Agent
affect agent effect = agent { -- x = x agent + x effect,
                              -- y = y agent + y effect,
                              lifepoints = lifepoints agent + lifepoints effect }
                              -- melee = melee agent + melee effect }

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
