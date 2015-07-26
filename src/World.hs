{-# LANGUAGE DeriveGeneric #-}
module World where

import Common.Prelude
import Data.Vect.Double
import Data.Vect.Double.Instances ()
import Simulation
import Geometry
import Graphics.Gloss
import GHC.Generics
import Generics.Deriving.Monoid

data World = World { getWalls :: [Wall]
                   , getWaves :: [Wave]
                   , getClick :: Maybe Point } deriving (Eq, Show, Generic)

data Wall = Wall Vec2 Vec2 deriving (Eq, Show, Read)

data Wave = Wave { getRadiusFn    :: RelativeTime -> Number
                 , getAngleSpanFn :: RelativeTime -> AngleSpan
                 , getRadius      :: Number
                 , getAngleSpan   :: AngleSpan
                 , getSource      :: Vec2 }

instance Eq Wave where
    Wave _ _ r1 a1 s1 == Wave _ _ r2 a2 s2 = r1 == r2 && a1 == a2 && s1 == s2

instance Show Wave where
    show (Wave _ _ r a s) = "Wave " ++ unwords [show r, show a, show s]

emptyWorld :: World
emptyWorld = World [] [] Nothing

drawWall :: Wall -> Picture
drawWall (Wall v1 v2) = Color white $ Line $ map vecToGloss [v1, v2]

drawWave :: Wave -> Picture
drawWave w = Color white $ Translate x y $ Arc (angleToGloss a) (angleToGloss b) (realToFrac $ getRadius w)
    where (AngleSpan a b) = getAngleSpan w
          (x, y)          = vecToGloss $ getSource w

drawWorld :: World -> Picture
drawWorld w = mconcat $ map drawWall (getWalls w) ++ map drawWave (getWaves w)
