module Draw.MeleeWarrior(warriorPictureFileList,drawWarrior) where

import qualified Data.Map as Map

import qualified Graphics.Gloss as Gloss

import Logic
import Draw.DrawTypes


figure = 1
figureSize = Vec 122 160
maceAnchorInFigure = Vec 28 129
maceAnchorRelPosInFigure = maceAnchorInFigure - 0.5 * figureSize

mace = 2
maceSize = Vec 42 166
maceAnchorInMace = Vec 16 154

negMaceAnchorRelPosInMace = 0.5 * maceSize - maceAnchorInMace

warriorPictureFileList :: PictureFileList
warriorPictureFileList = [
    (figure, "MeleeWarrior/Figure.bmp"),
    (mace, "MeleeWarrior/mace.bmp")
  ]

drawMace :: LoadedPictures -> Position -> Float -> Gloss.Picture
drawMace pics p angle = movePictureBy (p+maceAnchorRelPosInFigure) $ Gloss.rotate angle $
                            movePictureBy negMaceAnchorRelPosInMace $ pics mace

drawWarrior :: WarriorDrawer
drawWarrior pics (Warrior _ (Agent p _ _)) = Gloss.Pictures [movePictureBy p (pics figure), drawMace pics p 0.5]
