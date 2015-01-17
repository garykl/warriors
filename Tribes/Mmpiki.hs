module Tribes.Mmpiki where

import KiBuild
import Geometry
import qualified Data.Map as M


mmpiKi :: Intelligence
mmpiKi _ = melee (Vec 0 0)


mmpiWarrior :: Warrior
mmpiWarrior = Warrior (Soul MeleeWarrior 1 1 1)
                      (Agent (Pos 50 50) 10 Faineancing)


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = M.singleton "Heinz" (mmpiWarrior, mmpiKi)
