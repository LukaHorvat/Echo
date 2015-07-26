module Geometry where

import Common.Prelude
import Data.Fixed (mod')
import Simulation
import Data.Vect.Double
import Graphics.Gloss

newtype Angle = Angle Number deriving (Eq, Show, Read)

instance Ord Angle where
    compare (Angle x) (Angle y) | x > y && x - y <  pi = LT
                                | x > y && x - y >= pi = GT
                                | x < y && y - x <  pi = GT
                                | x < y && y - x >= pi = LT
                                | x == y               = EQ

data AngleSpan = AngleSpan Angle Angle deriving (Eq, Show, Read)

normalizeAngle :: Number -> Number
normalizeAngle x | x >= 2 * pi = x `mod'` (2 * pi)
                 | x <  0      = x `mod'` (2 * pi) + 2 * pi
                 | otherwise   = x

vecToGloss :: Vec2 -> Point
vecToGloss (Vec2 x y) = (realToFrac x, realToFrac y)

glossToVec :: Point -> Vec2
glossToVec (x, y) = Vec2 (realToFrac x) (realToFrac y)

angleToGloss :: Angle -> Float
angleToGloss (Angle x) = realToFrac . (* 180) . (/ pi) $ x

-- degToRad :: Number -> Number
-- degToRad = (* pi) . (/ 180)
