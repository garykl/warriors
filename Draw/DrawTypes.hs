module Draw.DrawTypes where

--import qualified Data.EnumMap as EnumMap
import qualified Data.Map as Map

import qualified Graphics.Gloss as Gloss

import Geometry
import Logic

{--- loading ---}
type LoadedPicturesKey = Int
type PictureFileList = [(LoadedPicturesKey, String)]
type LoadedPictures = LoadedPicturesKey -> Gloss.Picture
type AllWarriorPictures = Map.Map WarriorClass LoadedPictures
data AllLoadedPictures = AllLoadedPictures LoadedPictures AllWarriorPictures

{--- drawing ---}
type WarriorDrawer = LoadedPictures -> Warrior -> Gloss.Picture

movePictureBy :: Vector -> Gloss.Picture -> Gloss.Picture
movePictureBy (Vec x y) = Gloss.translate x y

movePictureTo :: Position -> Gloss.Picture -> Gloss.Picture
movePictureTo p = movePictureBy (p .-. positionOrigin)

scalePicture :: Float -> Gloss.Picture -> Gloss.Picture
scalePicture c = Gloss.scale c c
