module Draw.Draw(mainLoop) where

import qualified Data.Map as Map
import Data.List (sortBy)

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Logic

import Draw.DrawTypes
import qualified Draw.MeleeWarrior as MW

{--- load pictures for each warrior ---}
loadWarriorPictures :: WarriorClass -> IO LoadedPictures
loadWarriorPictures wc
        | wc==MeleeWarrior = MW.loadPictures
        | otherwise        = error ("No picture load function defined for WarriorClass \"" ++ show wc ++ "\"!")

loadGeneralPictures :: IO LoadedPictures
loadGeneralPictures = return []

loadAllPictures :: IO AllLoadedPictures
loadAllPictures = do
    generalPics <- loadGeneralPictures
    warriorPics <- sequence $ map loadWarriorPictures [minBound :: WarriorClass ..]
    return $ AllLoadedPictures generalPics (Map.fromList $ zip [minBound :: WarriorClass ..] warriorPics)

{--- drawing ---}
drawBackground :: LoadedPictures -> Picture
drawBackground pics = Blank

getWarriorDrawer :: WarriorClass -> WarriorDrawer
getWarriorDrawer wc
    | wc==MeleeWarrior = MW.drawWarrior
    | otherwise        = error ("No draw function defined for WarriorClass \"" ++ show wc ++ "\"!")

drawWarrior :: AllWarriorPictures -> Warrior -> Picture
drawWarrior pics w = getWarriorDrawer  wc (pics Map.! wc) w
    where (Warrior Soul{figureClass=wc} _) = w

drawWarriors :: AllWarriorPictures -> [Warrior] -> Picture
drawWarriors pics ws = Pictures $ map (drawWarrior pics) $ (sortBy (\(Warrior _ (Agent (Pos _ y1) _ _)) -> \(Warrior _ (Agent (Pos _ y2) _ _)) -> compare y1 y2)) ws

drawAll :: AllLoadedPictures -> Field -> Picture
drawAll (AllLoadedPictures generalPics warriorPics) (p1, p2) = Pictures [drawBackground generalPics, drawWarriors warriorPics ((Map.elems p1)++(Map.elems p2))]

{--- main loop ---}
mainLoop :: Field -> (Field -> Field) -> IO ()
mainLoop initialField performAction = do
        pics <- loadAllPictures
        simulate (InWindow "Warriors" (800, 550) (10, 10))
              green
              24
              initialField
              (drawAll pics)
              (\vp -> (\ti -> performAction))