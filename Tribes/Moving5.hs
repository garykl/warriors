module Tribes.Moving5 where


import KiBuild
import Geometry
import qualified Data.Map as M


mover :: Vector -> Intelligence
mover vec _ _ = move vec


intelligentWarrior :: Position -> (Warrior, Intelligence)
intelligentWarrior pos@(Pos x y) =
    (meleeWarrior pos, mover $ Pos 300 0 .-. pos)


(-<-) :: Ord k => k -> a -> M.Map k a -> M.Map k a
key -<- value = M.insert key value


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = ("Gerhardt" -<- intelligentWarrior (Pos (-300) 300))
        . ("Gustav" -<- intelligentWarrior (Pos (-300) 100))
        . ("Gernot" -<- intelligentWarrior (Pos (-300) (-100)))
        . ("Germann" -<- intelligentWarrior (Pos (-300) (-300)))
        $ M.empty
