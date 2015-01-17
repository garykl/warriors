module Observe where


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

    where distTo :: Position -> Warrior -> Float
          distTo (Pos x y) (Warrior _ agent) =
              vectorLength $ pos .-. position agent
