module Geometry where

-- do not make them Num, because of abs, signum and fromInteger
-- vectors are the differences of Positions
-- we could also define extra classes, but we don't have to

data Vector = Vec Float Float deriving Show

(|+|) :: Vector -> Vector -> Vector
Vec x1 y1 |+| Vec x2 y2 = Vec (x1+x2) (y1+y2)

(|-|) :: Vector -> Vector -> Vector
Vec x1 y1 |-| Vec x2 y2 = Vec (x1-x2) (y1-y2)

(|*|) :: Float -> Vector -> Vector
c |*| Vec x y = Vec (c*x) (c*y)

angleOfVector :: Vector -> Float
angleOfVector (Vec x y) = atan2 y x

radToDeg :: Float -> Float
radToDeg = (*(180/pi))

vectorLength :: Vector -> Float
vectorLength (Vec x y) = sqrt (x * x + y * y)

normalize :: Vector -> Vector
normalize vec@(Vec x y) =
    let l = vectorLength vec
    in  Vec (x / l) ( y / l)


data Position = Pos Float Float deriving Show

positionOrigin = Pos 0.0 0.0

(.+) :: Position -> Vector -> Position
(Pos x y) .+ (Vec dx dy) = Pos (x + dx) (y + dy)

(+.) :: Vector -> Position -> Position
v +. p = p .+ v

(.-) :: Position -> Vector -> Position
(Pos x y) .- (Vec dx dy) = Pos (x - dx) (y - dy)

(.-.) :: Position -> Position -> Vector
(Pos x1 y1) .-. (Pos x2 y2) = Vec (x1 - x2) (y1 - y2)


--class Addable a where
--    (|+|) :: a -> a -> a
--    (|-|) :: a -> a -> a
--
--instance Addable Position where
--    Pos x1 y1 |+| Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
--    Pos x1 y1 |-| Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
--
--instance Addable MeleeData where
--    MeleeData a1 p1 pos1 |+| MeleeData a2 p2 pos2 =
--        MeleeData (a1 + a2) (p1 + p2) (pos1 |+| pos2)
--    MeleeData a1 p1 pos1 |-| MeleeData a2 p2 pos2 =
--        MeleeData (a1 - a2) (p1 - p2) (pos1 |-| pos2)



