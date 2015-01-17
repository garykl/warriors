module Tribes.Garyki where


import KiBuild
import Geometry
import qualified Data.Map as M


garyKi :: Intelligence
garyKi _ = moveTo (Pos 50 50)


garyWarrior :: Warrior
garyWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                      (Agent (Pos 400 400) 10 Faineancing)


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = M.singleton "Gerhardt" (garyWarrior, garyKi)
