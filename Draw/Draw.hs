module Draw.Draw(mainLoop) where

import qualified Data.Map as Map
import Data.List (sortBy)
import qualified Data.Traversable as Traversable

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

import Geometry
import Logic

import Draw.DrawTypes
import qualified Draw.MeleeWarrior as MW
import qualified Draw.Wobble as Wobble

{--- load pictures for each warrior ---}
picturesBasePath = "Draw/"

generalPictureFileList :: PictureFileList
generalPictureFileList = []

warriorPictureFileList :: WarriorClass -> PictureFileList
warriorPictureFileList wc
        | wc == MeleeWarrior = MW.warriorPictureFileList
        | wc == Wobble = []
        | otherwise        = error ("No warrior picture file list defined for WarriorClass \"" ++ show wc ++ "\"!")

loadPictures :: PictureFileList -> IO LoadedPictures
loadPictures pfl = (Traversable.sequence $
              Map.fromList $
              map (\t -> (fst t, loadBMP $ (picturesBasePath ++ snd t))) pfl)
        >>= return.(Map.!) -- IO Map to IO function

loadAllPictures :: IO AllLoadedPictures
loadAllPictures = do
    generalPics <- loadPictures generalPictureFileList
    warriorPics <- sequence $ map (loadPictures . warriorPictureFileList) [minBound :: WarriorClass ..]
    return $ AllLoadedPictures generalPics (Map.fromList $ zip [minBound :: WarriorClass ..] warriorPics)

{--- drawing ---}
drawBackground :: LoadedPictures -> Picture
drawBackground pics = Blank

getWarriorDrawer :: WarriorClass -> WarriorDrawer
getWarriorDrawer wc
    | wc == MeleeWarrior = MW.drawWarrior
    | wc == Wobble = Wobble.drawWarrior
    | otherwise        = error ("No draw function defined for WarriorClass \"" ++ show wc ++ "\"!")

drawWarrior :: AllWarriorPictures -> Warrior -> Picture
drawWarrior pics w = getWarriorDrawer wc (pics Map.! wc) w
    where (Warrior Soul{figureClass=wc} _) = w

drawWarriors :: AllWarriorPictures -> [Warrior] -> Picture
drawWarriors pics ws = Pictures $ map (drawWarrior pics)
                                $ sortBy (\(Warrior _ (Agent (Pos _ y1) _ _))
                                           (Warrior _ (Agent (Pos _ y2) _ _))
                                                -> compare y1 y2) ws

drawAll :: AllLoadedPictures -> Field -> Picture
drawAll (AllLoadedPictures generalPics warriorPics) field =
    Pictures [drawBackground generalPics,
              drawWarriors warriorPics (fieldToListOfWarriors field)]

{--- main loop ---}
mainLoop :: Field -> (Field -> Field) -> IO ()
mainLoop initialField performAction = do
        pics <- loadAllPictures
        simulate (InWindow "Warriors" (800, 550) (10, 10))
              (makeColor 0.0 0.5 0.0 1.0)
              24
              initialField
              (drawAll pics)
              (\vp -> (\ti -> performAction))
