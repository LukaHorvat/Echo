module Geometry where

import Common.Prelude
import Data.Fixed (mod')
import Simulation
import Data.Vect.Double
import Graphics.Gloss

newtype Angle = Angle Number deriving (Eq, Show, Read, Ord)

ccwFrom :: Angle -> Angle -> Bool
(Angle x) `ccwFrom` (Angle y) | x > y && x - y <  pi = True
                              | x < y && y - x >= pi = True
                              | otherwise            = False

ccwEqFrom :: Angle -> Angle -> Bool
a1 `ccwEqFrom` a2 = a1 == a2 || a1 `ccwFrom` a2

cwFrom :: Angle -> Angle -> Bool
cwFrom a b = not $ ccwEqFrom a b

cwEqFrom :: Angle -> Angle -> Bool
cwEqFrom a b = not $ ccwFrom a b

instance Num Angle where
    fromInteger = mkAngle . fromInteger
    (+) = error "Angles can't be added"
    (*) = error "Angles can't be multiplied"
    abs = error "Angles don't have absolute values"
    signum = error "Angles don't have signum"
    negate = error "Angle can't be negated"

instance Fractional Angle where
    fromRational = mkAngle . fromRational
    recip = error "Angles don't have a reciprocal"

sin' :: Angle -> Number
sin' (Angle x) = sin x

cos' :: Angle -> Number
cos' (Angle x) = cos x

data AngleSpan = AngleSpan Angle Angle deriving (Eq, Show, Read)

mkAngle :: Number -> Angle
mkAngle = Angle . (`mod'` (2 * pi))

vecToGloss :: Vec2 -> Point
vecToGloss (Vec2 x y) = (realToFrac x, realToFrac y)

glossToVec :: Point -> Vec2
glossToVec (x, y) = Vec2 (realToFrac x) (realToFrac y)

angleToGloss :: Angle -> Float
angleToGloss (Angle x) = realToFrac . (* 180) . (/ pi) $ x

angleInSpan :: Angle -> AngleSpan -> Bool
angleInSpan ang (AngleSpan first second) =
    (ang `ccwEqFrom` first && second `ccwEqFrom` ang) ||
    (ang `ccwEqFrom` first && first `ccwFrom` second) ||
    (ang `cwEqFrom` second && second `cwFrom` first)
