module Draw.Wobble where


import qualified Graphics.Gloss as G
import qualified Draw.DrawTypes as DT
import qualified Logic as L
import Geometry


drawWarrior :: DT.WarriorDrawer
drawWarrior _ (L.Warrior soul agent) =
    let Pos x y = L.position agent
    in  G.translate x y $
            G.Pictures [G.color G.blue $ G.circleSolid (L.size soul),
                        G.color G.orange $ G.circle (L.size soul)]
