module Tribes.Mmpiki where

import KiBuild
import Geometry
import qualified Data.Map as M


attacker :: Vector -> Intelligence
attacker vec _ = melee vec


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



provide :: M.Map WarriorName (Warrior, Intelligence)
provide = M.insert "Heinz" (warrior (Pos 50 0), attacker (Vec (-100) 0))
        . M.insert "Angela" (warrior (Pos (-50) 0), attacker (Vec 100 0))
        $ M.empty
