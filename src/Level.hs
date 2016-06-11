module Level where

import           Data.Map.Strict      as Map
import           Prelude              hiding (Either (..), id, (.))
import           Control.Category
import           Control.Lens
import           Data.Default

import Coord

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data Tile = Floor |
            Wall deriving (Show, Eq)

instance Monoid Tile where
  mempty = Floor
  mappend x Floor = x
  mappend Floor x = x
  mappend x y = x

newtype LevelBuilder = LevelBuilder { asMap :: CoordMap Tile }

instance Monoid LevelBuilder where
  mempty = LevelBuilder Map.empty
  mappend x y = LevelBuilder $ Map.unionWith (<>) (asMap x) (asMap y)
  mconcat list = LevelBuilder $ Map.unionsWith (<>) (fmap asMap list)

setTile :: (Coord, Tile) -> LevelBuilder -> LevelBuilder
setTile (coord, tileType) (LevelBuilder builder) = LevelBuilder $ Map.insert coord tileType builder

mkTiles :: Tile -> [Coord] -> LevelBuilder
mkTiles tileType = Prelude.foldr set mempty where
  set coord = setTile (coord, tileType)

mkFloors :: Bounds -> LevelBuilder
mkFloors bounds = mkTiles Floor $ coordsWithin bounds

mkBounds :: Bounds -> LevelBuilder
mkBounds bounds = mkTiles Wall $ borderCoords bounds

conflict :: LevelBuilder -> LevelBuilder -> Bool
conflict (LevelBuilder builder1) (LevelBuilder builder2) = Map.size intersect /= 0 where
  intersect = Map.intersection builder1 builder2
