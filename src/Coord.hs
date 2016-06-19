{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Coord where

import qualified Control.Monad.Random as Random
import GHC.Generics
import Prelude hiding (Either(..), id, (.))
import Control.Category
import Data.Default
import Control.Lens
import Data.Map.Strict as Map
import Data.IntMap.Strict as IntMap
import qualified System.Random.Shuffle as Random
import Control.Monad

data Coord = Coord
    { _x :: Integer
    , _y :: Integer
    } deriving (Eq,Show,Read,Ord,Generic)

makeLenses ''Coord

data Bounds = Bounds
    { upper :: Coord
    , lower :: Coord
    } deriving (Eq,Show,Read,Ord,Generic)

instance Default Bounds where
    def = Bounds def def

instance Default Coord where
    def = Coord 0 0

type WorldCoord = Coord

type ScreenCoord = Coord

type DeltaCoord = Coord

type CoordMap a = Map.Map Coord a

instance Num Coord where
    (+) (Coord x y) (Coord x' y') = Coord (x + x') (y + y')
    (*) (Coord x y) (Coord x' y') = Coord (x * x') (y * y')
    negate (Coord x y) = Coord (-x) (-y)
    abs (Coord x y) = Coord (abs x) (abs y)
    signum (Coord x y) = Coord (signum x) (signum y)
    fromInteger i = Coord i' i'
      where
        i' = fromInteger i

quot :: Coord -> Coord -> Coord
quot (Coord ax ay) (Coord bx by) =
    Coord (ax `Prelude.quot` bx) (ay `Prelude.quot` by)

toPair :: Integral i => Coord -> (i, i)
toPair (Coord x y) = (fromInteger x, fromInteger y)

fromPair :: Integral i => (i, i) -> Coord
fromPair (x,y) = Coord (fromIntegral x) (fromIntegral y)


coordsWithin :: Bounds -> [Coord]
coordsWithin (Bounds (Coord lx ly) (Coord ux uy)) =
    [ Coord x y
    | x <- [lx .. ux]
    , y <- [ly .. uy] ]

borderCoords :: Bounds -> [Coord]
borderCoords (Bounds (Coord lx ly) (Coord ux uy)) =
    [ Coord x y
    | x <- [lx .. ux]
    , y <- [ly .. uy]
    , x == lx || x == ux || y == ly || y == uy ]

between
    :: (Ord a)
    => a -> a -> a -> Bool
between test lower upper = test >= lower && test < upper

within :: Coord -> Bounds -> Bool
within (Coord cx cy) (Bounds (Coord lx ly) (Coord ux uy)) = withinX && withinY
  where
    withinX = between cx lx ux
    withinY = between cy ly uy

coordSgn :: Coord -> Coord
coordSgn (Coord x y) = Coord sx sy
  where
    sx = signum x
    sy = signum y

randomWithin
    :: Random.MonadRandom m
    => Bounds -> m Coord
randomWithin b = Random.uniform (coordsWithin b)

-- Direction
data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Show,Read,Eq,Generic)

fromDirection :: Direction -> Coord
fromDirection d =
    case d of
        Left -> Coord 0 (-1)
        Right -> Coord 0 1
        Up -> Coord (-1) 0
        Down -> Coord 1 0

origin :: Coord
origin = Coord 0 0

flipOrder :: Coord -> Coord
flipOrder (Coord x y) = Coord y x

insetBounds :: Integer -> Bounds -> Bounds
insetBounds i (Bounds l u) = (Bounds l' u')
  where
    offset = (Coord i i)
    l' = l + offset
    u' = u - offset

splitCoordDelta :: Coord -> [Coord]
splitCoordDelta (Coord x y) =
    [ (Coord xs ys)
    | xs <- [x, 0]
    , ys <- [y, 0] ]

getRandomDirection
    :: (Random.MonadRandom m)
    => m Direction
getRandomDirection =
    Random.uniform [Coord.Left, Coord.Right, Coord.Down, Coord.Up]

randomDeltas
    :: (Random.MonadRandom m)
    => m [Coord]
randomDeltas =
    Random.shuffleM
        [ (Coord xs ys)
        | xs <- [-1, 0, 1]
        , ys <- [-1, 0, 1] ]

distance :: Floating a => Coord -> Coord -> a
distance (Coord x0 y0) (Coord x1 y1) = sqrt $ fromIntegral ((x1 - x0)^2 + (y1 - y0)^2)

line :: Coord -> Coord -> [Coord]
line c0 c1 = fromPair <$> line' (toPair c0) (toPair c1)

segment :: Coord -> Coord -> Int -> [Coord]
segment p1 p2 r = takeWhile lessThanR $ line p1 p2
  where
    lessThanR p3 = ceiling (distance p1 p3) <= fromIntegral r

circleCoords :: Integer -> Coord -> [Coord]
circleCoords r center = fmap (+ center) $ do
  x <- [-r..r]
  y <- [-r..r]
  guard (distance origin (Coord x y) < fromIntegral r)
  return (Coord x y)

line' :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line' p1@(x0, y0) (x1, y1) =
    let (dx, dy) = (x1 - x0, y1 - y0)
        xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
        yxStep b (x, y) = (x + signum dx * b, y + signum dy)
        (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                     | otherwise       = (abs dx, abs dy, yxStep)
        walk w xy = xy : walk (tail w) (step (head w) xy)
    in  walk (balancedWord p q 0) (x0, y0)
  where
    balancedWord :: Int -> Int -> Int -> [Int]
    balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
    balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)
