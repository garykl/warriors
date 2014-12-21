module Draw.DrawTypes where

--import qualified Data.EnumMap as EnumMap
import qualified Data.Map as Map

import Graphics.Gloss

import Logic (WarriorClass, Warrior)

{--- loading ---}
type LoadedPictures = [Picture]
type AllWarriorPictures = Map.Map WarriorClass LoadedPictures
data AllLoadedPictures = AllLoadedPictures LoadedPictures AllWarriorPictures

{--- drawing ---}
type WarriorDrawer = LoadedPictures -> Warrior -> Picture
