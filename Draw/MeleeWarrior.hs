module Draw.MeleeWarrior(warriorPictureFileList,drawWarrior) where

import qualified Data.Map as Map

import qualified Graphics.Gloss as Gloss

import Geometry
import Warrior
import Draw.DrawTypes

{--- general stuff ---}

figure = 1
figureSize = Vec 122 160
maceAnchorInFigure = Vec 28 (160-128)
maceAnchorRelPosInFigure = maceAnchorInFigure |-| (0.5 |*| figureSize)

mace = 2
maceSize = Vec 42 166
maceAnchorInMace = Vec 16 (166-154)
negMaceAnchorRelPosInMace = (0.5 |*| maceSize) |-| maceAnchorInMace

bam = 3
stars = 4

warriorPictureFileList :: PictureFileList
warriorPictureFileList = [
    (figure, "MeleeWarrior/Figure.bmp"),
    (mace, "MeleeWarrior/mace.bmp"),
    (bam, "MeleeWarrior/bam.bmp"),
    (stars, "MeleeWarrior/stars.bmp")
  ]

{--- mace stuff ---}
maceAngleForMovingAction = 30 -- in degrees

meleeMaceHitDuration :: Warrior -> Int
meleeMaceHitDuration warrior = round $ 0.05 * fromIntegral (liftSoul meleeDuration warrior)
meleeHitTimePlusMaceDelay warrior = meleeHitTime + meleeMaceHitDuration warrior
maxMaceAngleDiffForMeleeAction = 45 -- in degress
maceAngleOffsetForMeleeAction = 6 -- in drgrees

maceAngleDynamicsInDeg :: Warrior -> Int -> Float
maceAngleDynamicsInDeg warrior phase
  | phase<meleeHitTime
      = (1.0 - (fromIntegral phase)/(fromIntegral meleeHitTime))
              * maxMaceAngleDiffForMeleeAction
  | (phase>=meleeHitTime) && (phase < meleeHitTimePlusMaceDelay warrior)
      = 0
  | (phase >= meleeHitTimePlusMaceDelay warrior)
      = (fromIntegral $ phase - meleeHitTimePlusMaceDelay warrior)
              /(fromIntegral $ liftSoul meleeDuration warrior - meleeHitTimePlusMaceDelay warrior)
              * maxMaceAngleDiffForMeleeAction

maceAngleInDegForMeleeAction :: Warrior -> Int -> Float -> Float
maceAngleInDegForMeleeAction warrior phase baseAngle
  | ((n `mod` 2) == 0) =
      baseAngle + maceAngleOffsetForMeleeAction + maceAngleDynamicsInDeg warrior phase
  | ((n `mod` 2) == 1) =
      baseAngle - maceAngleOffsetForMeleeAction - maceAngleDynamicsInDeg warrior phase
  where n = 4 + round ( baseAngle/180 )

drawMaceAtAngleInDeg :: LoadedPictures -> Position -> Float -> Gloss.Picture
drawMaceAtAngleInDeg pics p angle =
            movePictureTo (p .+ maceAnchorRelPosInFigure) $
            Gloss.rotate (90-angle) $
            movePictureBy negMaceAnchorRelPosInMace $ pics mace

drawMace :: LoadedPictures -> Warrior -> Gloss.Picture
drawMace pics warrior@(Warrior _ (Agent pos _ (MeleeAttacking _ phase meleeVec))) =
            drawMaceAtAngleInDeg pics pos $
            maceAngleInDegForMeleeAction warrior phase $
            radToDeg $ angleOfVector (meleeVec |-| maceAnchorRelPosInFigure)
drawMace pics (Warrior _ (Agent pos _ _)) =
            drawMaceAtAngleInDeg pics pos maceAngleForMovingAction

{--- stars stuff ---}
meleeStarsDuration :: Warrior -> Int
meleeStarsDuration warrior = round $ 0.15 * fromIntegral (liftSoul meleeDuration warrior)
meleeHitTimePlusStarsDelay warrior = meleeHitTime + meleeStarsDuration warrior
starsRadiusRatioOffset = 0.4

drawStars :: LoadedPictures -> Warrior -> Gloss.Picture
drawStars pics warrior@(Warrior _ (Agent pos _ (MeleeAttacking _ phase meleeVec)))
  | (phase>=meleeHitTime) && (phase <= meleeHitTimePlusStarsDelay warrior) = movePictureTo (pos .+ meleeVec) $
      Gloss.Pictures [ pics bam,
        scalePicture (starsRadiusRatioOffset +
            (1.0-starsRadiusRatioOffset)*(fromIntegral $ phase - meleeHitTime)
              /(fromIntegral $ meleeStarsDuration warrior)) $ pics stars
      ]
  | otherwise = Gloss.Blank
drawStars pics _ = Gloss.Blank


{--- put everything together ---}
drawWarrior :: WarriorDrawer
drawWarrior pics warrior@(Warrior _ a@(Agent p _ _)) =
    Gloss.rotate (if warriorDead warrior
        then 90
        else 0) $
      Gloss.Pictures [
        movePictureTo p (pics figure),
        drawStars pics warrior,
        drawMace pics warrior
      ]
