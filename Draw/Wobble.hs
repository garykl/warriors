module Draw.Wobble where


import qualified Graphics.Gloss as G
import qualified Draw.DrawTypes as DT
import qualified Warrior as W
import Geometry


drawWarrior :: DT.WarriorDrawer
drawWarrior _ (W.Warrior soul agent) =

  let Pos x y = W.position agent
  in  G.translate x y $

    case W.actionStatus agent of

        W.Faineancing ->
            G.color (G.greyN 0.5) theCircle

        W.Moving target ->
            elongate (angleOfVector target) theCircle

        W.MeleeAttacking _ _ target ->
            G.color G.red $ elongate (angleOfVector target) theCircle

    where

      theCircle :: G.Picture
      theCircle =
          G.Pictures [G.color G.blue $ G.circleSolid (W.size soul),
                      G.color G.orange $ G.circle (W.size soul)]

      elongate :: Float -> G.Picture -> G.Picture
      elongate angle pic =
          G.rotate (negate . toDegree $ angle) $ G.scale 1.3 0.7 pic

      toDegree :: Float -> Float
      toDegree angle = 180 * angle / pi
