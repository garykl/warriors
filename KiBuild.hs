module KiBuild (Intelligence, Warrior(Warrior), Soul(Soul), Agent(Agent),
                ActionStatus(Faineancing), WarriorClass(..), WarriorName,
                moveTo, melee) where


import Logic
import Geometry


moveTo :: Position -> Action
moveTo = MoveTo


melee :: Vector -> Action
melee = Melee
