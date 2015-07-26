module Main where

import Common.Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Geometry
import World

main :: IO ()
main = play (InWindow "Nice Window" (800, 500) (10, 10))
            black
            60
            emptyWorld
            drawWorld
            makeWalls
            (flip const)


makeWalls :: Event -> World -> World
makeWalls (EventKey (MouseButton LeftButton) Up _ new) w =
    case getClick w of
        Just old -> w { getWalls = Wall (glossToVec old) (glossToVec new) : getWalls w
                      , getClick  = Nothing }
        Nothing  -> w { getClick = Just new }
makeWalls _ w = w
