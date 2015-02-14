module Tribes.Mmpiki where

import KiBuild
import Geometry
import qualified Data.Map as M


attacker :: Vector -> Intelligence
attacker vec _ _ = melee vec


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = M.insert "Heinz" (meleeWarrior (Pos 30 0), attacker (Vec (-100) 0))
        . M.insert "Angela" (meleeWarrior (Pos (-30) 0), attacker (Vec 100 0))
        $ M.empty
