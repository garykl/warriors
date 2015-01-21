module Tribes.Moving5 where


import KiBuild
import Geometry
import qualified Data.Map as M


mover :: Position -> Intelligence
mover pos _ = moveTo pos


warrior :: Position -> Warrior
warrior pos = Warrior
    Soul { figureClass = Wobble,
           size = 60,
           velocity = 1,
           vitality = 1,
           strength = 1 }
    Agent { position = pos,
            lifepoints = 10,
            actionStatus = Faineancing }


intelligentWarrior :: Position -> (Warrior, Intelligence)
intelligentWarrior pos@(Pos x y) =
    (warrior pos, mover (Pos 300 0))


(-<-) :: Ord k => k -> a -> M.Map k a -> M.Map k a
key -<- value = M.insert key value


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = ("Gerhardt" -<- intelligentWarrior (Pos (-300) 300))
        . ("Gustav" -<- intelligentWarrior (Pos (-300) 100))
        . ("Gernot" -<- intelligentWarrior (Pos (-300) (-100)))
        . ("Germann" -<- intelligentWarrior (Pos (-300) (-300)))
        $ M.empty
