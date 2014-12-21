module Draw.MeleeWarrior(loadPictures,drawWarrior) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Logic
import Draw.DrawTypes

--main :: IO ()
--main = do
--    pic <- loadBMP "Figure.bmp"
----    display (InWindow "Warriors" (400, 550) (10, 10))
----            white
----            pic
--    simulate (InWindow "Warriors" (800, 550) (10, 10))
--              green
--              24
--              0
--              (draw pic)
--              timestep

loadPictures :: IO LoadedPictures
loadPictures = sequence [loadBMP "Draw/MeleeWarrior/Figure.svg.bmp"]

drawWarrior :: WarriorDrawer
drawWarrior pics (Warrior _ (Agent (Pos x y) _ _)) = translate x y (head pics)
