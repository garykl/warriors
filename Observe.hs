module Observe where


import qualified Warrior as W
import Logic
import Geometry
import qualified NestedMap as N
import Data.List (sortBy)
import Data.Function (on)



warriorDistances :: Position -> Field -> [WarriorIdentifier]
warriorDistances pos field =

    let warriors = N.values field
        dists = map (distTo pos) warriors

    in  map fst
            $ sortBy (compare `on` snd)
                 $ zip (N.keys field) dists

    where distTo :: Position -> W.Warrior -> Float
          distTo (Pos x y) (W.Warrior _ agent) =
              vectorLength $ pos .-. W.position agent


fieldCross :: Field -> W.Warrior -> Bool
fieldCross field warrior = any (W.warriorCross warrior) $ N.elemsDeep field
