module Logic where

import qualified Data.Map as M
import qualified NestedMap as N

import Geometry
import Warrior


-- | when performing an @Action@, an @Effect@ is generated, that is applied to
-- the warriors @Agent@.
data Effect = Effect (Agent -> Agent)
type Effects = N.Nmap TribeName WarriorName [Effect]


-- | @emptyEffect@ means no effect
emptyEffect :: Effect
emptyEffect = Effect id


emptyEffects :: [WarriorIdentifier] -> Effects
emptyEffects is = composeAll (map ((N.->-) []) is) N.empty


-- | effects are added by composing immediate effects, later effects, and so on.
composeEffects :: Effects -> Effects -> Effects
composeEffects = N.zipWith listCompose
  where
    listCompose :: [Effect] -> [Effect] -> [Effect]
    listCompose es1 es2 =
        let maxnum = max (length es1) (length es2)
        in  zipWith (\(Effect e1) (Effect e2) -> Effect (e1 . e2))
                    (constrainLength maxnum emptyEffect es1)
                    (constrainLength maxnum emptyEffect es2)



applyEffects :: Effects -> Field -> (Effects, Field)
applyEffects effects field = N.unzip $ N.zipWith applyEffectList effects field
  where
    applyEffectList :: [Effect] -> Warrior -> ([Effect], Warrior)
    applyEffectList [] warrior = ([], warrior)
    applyEffectList effects warrior =
        (tail effects, applyEffect (head effects) warrior)
            where applyEffect :: Effect -> Warrior -> Warrior
                  applyEffect (Effect effect) (Warrior soul agent) =
                      Warrior soul (effect agent)


-- | cut a list if it is too long or fill it with default elements if it is
-- too short
constrainLength :: Int -> a -> [a] -> [a]
constrainLength n l ll =
    let num = length ll
    in  if n > num then ll ++ replicate (n - num) l
                   else take n ll


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
type Intelligence = Environment -> Action
type Intelligences = N.Nmap TribeName WarriorName Intelligence


-- | @Warrior@s interact with each other and with themselves via @Action@s.
-- @Action@s are converted into @Effect@s, which are then added to the
-- @Agent@s.
data Action = Melee Vector  -- relative to attackers position
            | MoveTo Position -- absolute position of the aim


type TribeName = String

-- | the @Field@ is given by considering two Teams.
type Field = N.Nmap TribeName WarriorName Warrior -- M.Map TribeName Tribe


-- | a @Warrior@ on the @Field@ can be identified by its name (the key of the
-- @Tribe@ @M.Map@) and the @Tribe@ number (tribe == fst field <=> 1, ...).
type WarriorIdentifier = (TribeName, WarriorName)


-- | compose any number of function, for being executed in order.
composeAll :: [a -> a] -> a -> a
composeAll [] a = a
composeAll (f : fs) a = foldr (.) f fs a

composeAllSnd :: [a -> (b, a)] -> a -> a
composeAllSnd fs = composeAll $ map (snd .) fs


chooseAction :: WarriorIdentifier -> Intelligences -> Field -> Action
chooseAction wid intelligence field =
    intelligence N.! wid $ getEnvironment wid field



-- | create the @Environment@ of a @Warrior@. This should depend on the
-- @Agent@, which dicides how well it is recognized.
-- TODO: implement the idea. current state: everything is recognized as it is
-- and only sorted by the tribe.
getEnvironment :: WarriorIdentifier -> Field -> Environment
getEnvironment wid@(tribename, _) field =  -- TODO: warriorname will be used to determine the blurring!
    Env (N.elem tribename field)
       $ N.elems $ N.deleteRough tribename field


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
