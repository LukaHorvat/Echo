{-# LANGUAGE DeriveGeneric #-}
module World where

import Common.Prelude
import Data.Vect.Double hiding (project)
import Data.Vect.Double.Instances ()
import Simulation
import Geometry
import Graphics.Gloss
import GHC.Generics
import Generics.Deriving.Monoid
import Control.Monad.State

data World = World { getWalls :: [Wall]
                   , getWaves :: [Wave]
                   , getClick :: Maybe Point
                   , debugPcs :: [Picture] } deriving (Eq, Show, Generic)

data Wall = Wall Vec2 Vec2 deriving (Eq, Show, Read)

data Wave = Wave { getExpandSpeed :: Number
                 , getRadius      :: Number
                 , getAngleSpan   :: AngleSpan
                 , getSource      :: Vec2 }
                 deriving (Eq, Show, Read)

emptyWorld :: World
emptyWorld = World [] [] Nothing []

drawWall :: Wall -> Picture
drawWall (Wall v1 v2) = Color white $ Line $ map vecToGloss [v1, v2]

drawWave :: Wave -> Picture
drawWave w = Color white $ Translate x y $ Arc (angleToGloss a) (angleToGloss b) (realToFrac $ getRadius w)
    where (AngleSpan a b) = getAngleSpan w
          (x, y)          = vecToGloss $ getSource w

drawWorld :: World -> Picture
drawWorld w = mconcat $ map drawWall (getWalls w) ++ map drawWave (getWaves w)

type PolarWall = [(Angle, Number)]

toPolar :: Vec2 -> Wall -> PolarWall
toPolar s (Wall a b)
    | aphi `ccwFrom` bphi = [(bphi, br), (aphi, ar)]
    | otherwise           = [(aphi, ar), (bphi, br)]
    where aphi = mkAngle $ angle s a
          bphi = mkAngle $ angle s b
          ar = distance s a
          br = distance s b

getPolarAngleSpan :: PolarWall -> AngleSpan
getPolarAngleSpan [(aphi, ar), (bphi, br)] = AngleSpan aphi bphi

cutAtZero :: Vec2 -> Wall -> [Wall]
cutAtZero (Vec2 sx sy) (Wall a@(Vec2 ax ay) b@(Vec2 bx by))
    | shouldSplit = [Wall a s', Wall s' b]
    | otherwise   = [Wall a b]
    where u = (sy - ay) / denom
          s' = a + u *& (b - a)
          numer = (sy - ay) * (bx - ax) - (sx - ax) * (by - ay)
          denom = by - ay
          shouldSplit = signum numer == signum denom && u > 0 && u < 1

project :: Vec2 -> Wall -> Vec2
project s (Wall a b) = (((s - a) &. dir) / n) *& dir + a
    where n = normsqr (b - a)
          dir = b - a

cutAtProjection :: Vec2 -> Wall -> [Wall]
cutAtProjection s (Wall a b)
    | shouldSplit = [Wall a p, Wall p b]
    | otherwise   = [Wall a b]
    where p = project s (Wall a b)
          (Vec2 px py) = p - a
          (Vec2 wx wy) = b - a
          shouldSplit = signum px == signum wx && signum py == signum wy
                     || abs px    <  abs wx    && abs py    <  abs wy

splitAtAngle :: Vec2 -> PolarWall -> Angle -> [PolarWall]
splitAtAngle (Vec2 sx sy) [(aphi, ar), (bphi, br)] phi = result
    where ax = sx + ar * cos' aphi
          ay = sy + ar * sin' aphi
          bx = sx + br * cos' bphi
          by = sy + br * sin' bphi
          frac = (by - ay) / (bx - ax)
          numer = sy - ay - frac * (sx - ax)
          denom = frac * cos' phi - sin' phi
          r = numer / denom
          result = [[(aphi, ar), (phi, r)], [(phi, r), (bphi, br)]]

polarToPoints :: (Int, PolarWall) -> [((Number, Number), Int)]
polarToPoints (n, [(Angle aphi, ar), (Angle bphi, br)])
    | bphi == 0 = [((aphi, ar), n), ((2 * pi, br), n)]
    | otherwise = [((aphi, ar), n), ((bphi, br), n)]

addWave :: Vec2 -> Number -> AngleSpan -> State World [Wave]
addWave center initialRad initialAngle = do
    walls <- gets getWalls
    let splitWalls = walls >>= cutAtZero center >>= cutAtProjection center
    let polars = zip [0..] $ map (toPolar center) splitWalls
    let points = sort $ concatMap polarToPoints polars
    let 
    return undefined
