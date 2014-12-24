module Draw.MeleeWarrior(warriorPictureFileList,drawWarrior) where

import qualified Data.Map as Map

import qualified Graphics.Gloss as Gloss

import Geometry
import Logic
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
maceAngleForMovingAction = 30

meleeMaceHitDuration = round $ 0.15 * fromIntegral meleeDuration
meleeHitTimePlusMaceDelay = meleeHitTime + meleeMaceHitDuration
maxMaceAngleDiffForMeleeAction = 45
maceAngleOffsetForMeleeAction = 8

maceAngleDynamics :: Int -> Float
maceAngleDynamics phase
  | phase<meleeHitTime
      = (1.0 - (fromIntegral phase)/(fromIntegral meleeHitTime))
              * maxMaceAngleDiffForMeleeAction
  | (phase>=meleeHitTime) && (phase<meleeHitTimePlusMaceDelay)
      = 0
  | (phase>=meleeHitTimePlusMaceDelay)
      = (fromIntegral $ phase - meleeHitTimePlusMaceDelay)
              /(fromIntegral $ meleeDuration - meleeHitTimePlusMaceDelay)
              * maxMaceAngleDiffForMeleeAction

maceAngleInDegForMeleeAction :: Int -> Float -> Float
maceAngleInDegForMeleeAction phase baseAngle
  | ((n `mod` 2) == 0) = baseAngle
      + maceAngleOffsetForMeleeAction + maceAngleDynamics phase
  | ((n `mod` 2) == 1) = baseAngle
      - maceAngleOffsetForMeleeAction - maceAngleDynamics phase
  where n = 4 + round ( baseAngle/180 )

drawMaceAtAngle :: LoadedPictures -> Position -> Float -> Gloss.Picture
drawMaceAtAngle pics p angle = movePictureTo (p .+ maceAnchorRelPosInFigure) $
                            Gloss.rotate (90-angle) $
                            movePictureBy negMaceAnchorRelPosInMace $ pics mace

drawMace :: LoadedPictures -> Agent -> Gloss.Picture
drawMace pics (Agent pos _ Moving) = drawMaceAtAngle pics pos maceAngleForMovingAction
drawMace pics (Agent pos _ (MeleeAttacking _ phase meleeVec)) = drawMaceAtAngle pics pos $
            maceAngleInDegForMeleeAction phase $ radToDeg $
            angleOfVector (meleeVec |-| maceAnchorRelPosInFigure)

{--- stars stuff ---}
meleeStarsDuration = round $ 0.15 * fromIntegral meleeDuration
meleeHitTimePlusStarsDelay = meleeHitTime + meleeStarsDuration
starsRadiusRatioOffset = 0.4

drawStars :: LoadedPictures -> Agent -> Gloss.Picture
drawStars pics (Agent pos _ (MeleeAttacking _ phase meleeVec))
  | (phase>=meleeHitTime) && (phase<=meleeHitTimePlusStarsDelay) = movePictureTo (pos .+ meleeVec) $
      Gloss.Pictures [ pics bam,
        scalePicture (starsRadiusRatioOffset +
            (1.0-starsRadiusRatioOffset)*(fromIntegral $ phase - meleeHitTime)
              /(fromIntegral meleeStarsDuration)) $ pics stars
      ]
  | otherwise = Gloss.Blank
drawStars pics _ = Gloss.Blank


{--- put everything together ---}
drawWarrior :: WarriorDrawer
drawWarrior pics (Warrior _ a@(Agent p _ _)) = Gloss.Pictures [
    movePictureTo p (pics figure),
    drawStars pics a,
    drawMace pics a
  ]
