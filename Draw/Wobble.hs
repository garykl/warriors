module Draw.Wobble where


import qualified Graphics.Gloss as G
import qualified Draw.DrawTypes as DT
import qualified Warrior as W
import Geometry


drawWarrior :: DT.WarriorDrawer
drawWarrior _ (W.Warrior soul agent) =
    let Pos x y = W.position agent
    in  G.translate x y $
            G.Pictures [G.color G.blue $ G.circleSolid (W.size soul),
                        G.color G.orange $ G.circle (W.size soul)]
