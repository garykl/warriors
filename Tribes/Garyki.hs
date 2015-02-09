module Tribes.Garyki where


import KiBuild
import Geometry
import qualified Data.Map as M


mover :: Vector -> Intelligence
mover vec _ = move vec


warrior :: Position -> Warrior
warrior pos = Warrior
    Soul { figureClass = MeleeWarrior,
           size = 1,
           velocity = 1,
           vitality = 1,
           strength = 1 }
    Agent { position = pos,
            lifepoints = 10,
            actionStatus = Faineancing }


intelligentWarrior :: Position -> (Warrior, Intelligence)
intelligentWarrior pos@(Pos x y) =
    (warrior pos, mover $ Pos (-y) x .-. pos)


(-<-) :: Ord k => k -> a -> M.Map k a -> M.Map k a
key -<- value = M.insert key value


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = ("Gerhardt" -<- intelligentWarrior (Pos 300 0))
        . ("Gustav" -<- intelligentWarrior (Pos (-300) 0))
        . ("Gernot" -<- intelligentWarrior (Pos 0 300))
        . ("Germann" -<- intelligentWarrior (Pos 0 (-300)))
        $ M.empty
