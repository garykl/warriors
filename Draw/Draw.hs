module Draw.Draw(mainLoop) where

import qualified Data.EnumMap as EnumMap

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Logic (FieldState,Team,Warrior,WarriorClass)

import qualified MeleeWarrior as MW

{--- load pictures for each warrior ---}
type LoadedPictures = [Picture]
type PictureSetLoader = IO LoadedPicture
data AllLoadedPictures = AllLoadedPictures LoadedPictures (EnumMap.EnumMap WarriorClass LoadedPictures)

pictureLoadFunctions wc
        | wc==MeleeWarrior = MW.loadPictures
        | otherwise        = error

loadGeneralPictures :: IO LoadedPictures
loadGeneralPictures = return :: IO []

loadAllPictures :: IO AllLoadedPictures
loadAllPictures = do
    generalPics <- loadGeneralPictures
    warriorPics <- sequence $   assocs [minBound :: WarriorClass..]
    LoadedPictures

{--- drawing ---}
warriorDrawFunctions = undefined

drawWarrior :: Warrior -> Picture
drawWarrior _ = undefined

drawAll :: AllLoadedPictures -> FieldState -> Picture
drawAll _ _ = undefined

{--- main loop ---}
mainLoop :: (FieldState -> FieldState) -> IO ()
mainLoop = do
        pics <- loadPics
        simulate (InWindow "Warriors" (800, 550) (10, 10))
              green
              24
              0
              (drawAll pics)
              (\vp -> (\ti -> timestep))
