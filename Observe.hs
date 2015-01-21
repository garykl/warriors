module Observe where


import Logic
import Geometry
import qualified NestedMap as N
import Data.List (sortBy)
import Data.Function (on)


warriorDead :: Warrior -> Bool
warriorDead (Warrior _ agent) = lifepoints agent <= 0


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


agentDistance :: Agent -> Agent -> Float
agentDistance a1 a2 = vectorLength $ position a1 .-. position a2


warriorCross :: Warrior -> Warrior -> Bool
warriorCross (Warrior sl1 ag1) (Warrior sl2 ag2) =
    agentDistance ag1 ag2 < size sl1 + size sl2


fieldCross :: Field -> Warrior -> Bool
fieldCross field warrior = any (warriorCross warrior) $ N.elemsDeep field
