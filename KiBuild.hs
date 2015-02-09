module KiBuild (Intelligence, Warrior(Warrior), Soul(..), Agent(..),
                ActionStatus(Faineancing), WarriorClass(..), WarriorName,
                move, melee, meleeWarrior) where


import Warrior
import Logic
import Geometry


move :: Vector -> Action
move = Move


melee :: Vector -> Action
melee = Melee


-- at some point, it could turn out to be useful to be having had built up
-- an intelligence arithmetic.
-- TODO: think about it: shall Intelligence be a proper type?

combineIdeas :: (Environment -> Bool) -> Intelligence -> Intelligence
             -> Intelligence
combineIdeas f i1 i2 e = if f e then i1 e else i2 e

(>-<) :: Intelligence -> Intelligence -> (Environment -> Bool) -> Intelligence
i1 >-< i2 = \f -> combineIdeas f i1 i2


complement :: (Action -> Action) -> Intelligence -> Intelligence
complement f i e = f $ i e
