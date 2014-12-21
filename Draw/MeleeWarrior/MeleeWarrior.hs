module MeleeWarrior where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


main :: IO ()
main = do
    pic <- loadBMP "Figure.svg.bmp"
    display (InWindow "Warriors" (400, 550) (10, 10))
            white
            pic
    -- simulate (InWindow "Warriors" (400, 550) (10, 10))
    --          white
    --          24
    --          0
    --          (draw pic)
    --          timestep

draw :: Picture -> Float -> Picture
draw pic n = translate n n pic

timestep :: ViewPort -> Float -> Float -> Float
timestep _ _ = (+ 1)