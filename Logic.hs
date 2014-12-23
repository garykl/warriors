module Logic where

import qualified Data.Map as M
import qualified NestedMap as N


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

emptyMeleeData :: MeleeData
emptyMeleeData = MeleeData 0 0 $ Pos 0 0

-- | the @Agent@ is the suffering part during the simulation. Values may
-- constantly change, due to attack, spells or movements.
data Agent = Agent { position :: Position,
                     lifepoints :: Float,
                     melee :: MeleeData }

-- | when performing an @Action@, an @Effect@ is generated, that is applied to
-- the warriors @Agent@.
data Effect = Effect (Agent -> Agent)


-- | @emptyEffect@ means no effect
emptyEffect :: Effect
emptyEffect = Effect id

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

type WarriorName = String

-- | the @Tribe@ is a collection of @Warrior@s. They are thought to be fighting
-- against another @Tribe@. Additionally, the @Tribe@ gives each Warrior its
-- identity.
type Tribe = M.Map WarriorName Warrior

-- | each @Warrior@ sees its own environment, that consists of its own @Team@
-- and the enemy team.
data Environment = Env Tribe [Tribe]

-- | @Intelligence@ is, when you are able to conclude different actions for
-- different @Environment@s.
type Intelligence = WarriorName -> Environment -> Action

type Intelligences = M.Map TribeName Intelligence


-- | @Warrior@s interact with each other and with themselves via @Action@s.
-- @Action@s are converted into @Effect@s, which are then added to the
-- @Agent@s.
data Action = Melee Position
            | MoveTo Position


type TribeName = String

-- | the @Field@ is given by considering two Teams.
type Field = N.Nmap TribeName WarriorName Warrior -- M.Map TribeName Tribe


-- | a @Warrior@ on the @Field@ can be identified by its name (the key of the
-- @Tribe@ @M.Map@) and the @Tribe@ number (tribe == fst field <=> 1, ...).
type WarriorIdentifier = (TribeName, WarriorName)


inititialWarrior :: Warrior
inititialWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                          (Agent (Pos 0 0) 1 (MeleeData 0 0 (Pos 0 0)))

initialField :: Field
initialField = N.singleton "Holzfaeller" "Heinz" inititialWarrior


hhh :: Field -> Field
hhh field =
    let Warrior soul agent = field N.! ("Holzfaeller", "Heinz")
        MeleeData amount phase position = melee agent
    in  N.singleton "Holzfaeller" "Heinz"
            $ Warrior soul
                    $ agent {melee = MeleeData amount
                                         ((phase + 1) `mod` meleeDuration)
                                         position }


-- | each @Warrior@ on the @Field@ can try to act appropriately.
performTimestep :: Intelligences -> Field -> Field
performTimestep intelligences field =
    let performs = map (`chooseAndPerformAction` intelligences) $ getOrder field
    in  composeAll performs field


-- | compose any number of function, for being executed in order.
composeAll :: [a -> a] -> a -> a
composeAll [] a = a
composeAll (f : fs) a = foldr (.) f fs a


chooseAction :: WarriorIdentifier -> Intelligence -> Field -> Action
chooseAction wid@(tribename, warriorname) intelligence field =
    let environment = getEnvironment tribename field
    in  intelligence warriorname environment

chooseAndPerformAction :: WarriorIdentifier -> Intelligences -> Field -> Field
chooseAndPerformAction wid@(tribename, _) intelligences field =
    let action = chooseAction wid $ intelligences M.! tribename
    in  field


-- | for knowing what the @Effect@ of an @Action@ is, we need the @Field@,
-- the @Warrior@ tht performs the @Action@ and the @Action@ itself. Multiple
-- target may be affected (-> keys are @WarriorIdentifier@s) and the effect
-- may be in the future (-> values are lists of @Effect@s).
actionToEffects :: Field -> Warrior -> Action
                -> N.Nmap TribeName WarriorName [Effect]
actionToEffects field warrior (Melee _) = N.empty
actionToEffects field warrior (MoveTo _) = N.empty


-- | create the @Environment@ of a @Warrior@. This should depend on the
-- @Agent@, which dicides how well it is recognized.
-- TODO: implement the idea. current state: everything is recognized as it is
-- and only sorted by the tribe.
getEnvironment :: TribeName -> Field -> Environment
getEnvironment name field = Env (N.elem name field)
                              $ N.elems $ N.deleteRough name field


-- | extract a @Warrior@ out of the @Field@
getWarrior :: WarriorIdentifier -> Field -> Warrior
getWarrior wid field = field N.! wid

-- | @Warrior@s may be faster of slower.
-- TODO: implement this idea. current status: random order of @Tribe@ 1 followed
-- by random order of @Tribe@ 2.
getOrder :: Field -> [WarriorIdentifier]
getOrder = N.keys


fieldToListOfWarriors :: Field -> [Warrior]
fieldToListOfWarriors = N.elemsDeep
