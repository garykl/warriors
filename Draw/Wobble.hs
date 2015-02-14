module Draw.Wobble where


import qualified Graphics.Gloss as G
import qualified Draw.DrawTypes as DT
import qualified Warrior as W
import Geometry


drawWarrior :: DT.WarriorDrawer
drawWarrior _ warrior@(W.Warrior soul agent) =

  let Pos x y = W.position agent
  in  G.translate x y $

   if W.warriorDead warrior then theCircle (G.greyN 0.1)
    else

     case W.actionStatus agent of

         W.Faineancing ->
             theCircle (G.greyN 0.5)

         W.Moving target ->
             elongate (angleOfVector target) $ theCircle G.blue

         W.MeleeAttacking _ _ target ->
             elongate (angleOfVector target) $ theCircle G.red

    where

      theCircle :: G.Color -> G.Picture
      theCircle color =
          G.Pictures [G.color color $ G.circleSolid (W.size soul),
                      G.color G.orange $ G.circle (W.size soul)]

      elongate :: Float -> G.Picture -> G.Picture
      elongate angle pic =
          G.rotate (negate . toDegree $ angle) $ G.scale 1.3 0.7 pic

      toDegree :: Float -> Float
      toDegree angle = 180 * angle / pi
