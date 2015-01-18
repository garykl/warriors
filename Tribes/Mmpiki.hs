module Tribes.Mmpiki where

import KiBuild
import Geometry
import qualified Data.Map as M


mmpiKi :: Vector -> Intelligence
mmpiKi vec _ = melee vec


mmpiWarrior :: Position -> Warrior
mmpiWarrior pos = Warrior (Soul MeleeWarrior 1 1 1)
                          (Agent pos 10 Faineancing)


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = M.insert "Heinz" (mmpiWarrior (Pos 50 0), mmpiKi (Vec (-50) 0))
        . M.insert "Angela" (mmpiWarrior (Pos (-50) 0), mmpiKi (Vec 50 0))
        $ M.empty
