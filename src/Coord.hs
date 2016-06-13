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

toPair :: Coord -> (Integer, Integer)
toPair (Coord x y) = (x, y)

fromPair :: (Integer, Integer) -> Coord
fromPair (x,y) = Coord x y

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
