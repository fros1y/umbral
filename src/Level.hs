module Level where

import Control.Category
import Control.Lens
import Data.Default
import Data.Map.Strict as Map
import Prelude hiding (Either(..), id, (.))
import Coord
import Entity
import GameState

(<>)
    :: Monoid m
    => m -> m -> m
(<>) = mappend

data Tile
    = Floor
    | Wall
    deriving (Show,Eq)

instance Monoid Tile where
    mempty = Floor
    mappend x Floor = x
    mappend Floor x = x
    mappend x y = x

newtype LevelBuilder = LevelBuilder
    { asMap :: CoordMap Tile
    }

instance Monoid LevelBuilder where
    mempty = LevelBuilder Map.empty
    mappend x y = LevelBuilder $ Map.unionWith (<>) (asMap x) (asMap y)
    mconcat list = LevelBuilder $ Map.unionsWith (<>) (fmap asMap list)

setTile :: (Coord, Tile) -> LevelBuilder -> LevelBuilder
setTile (coord,tileType) (LevelBuilder builder) =
    LevelBuilder $ Map.insert coord tileType builder

mkTiles :: Tile -> [Coord] -> LevelBuilder
mkTiles tileType = Prelude.foldr set mempty
  where
    set coord = setTile (coord, tileType)

mkFloors :: Bounds -> LevelBuilder
mkFloors bounds = mkTiles Floor $ coordsWithin bounds

mkBounds :: Bounds -> LevelBuilder
mkBounds bounds = mkTiles Wall $ borderCoords bounds

mkRoom :: Bounds -> LevelBuilder
mkRoom bounds = (mkFloors bounds) <> (mkBounds bounds)

conflict :: LevelBuilder -> LevelBuilder -> Bool
conflict (LevelBuilder builder1) (LevelBuilder builder2) =
    Map.size intersect /= 0
  where
    intersect = Map.intersection builder1 builder2

mergeLevelBuilder :: LevelBuilder -> GameState -> GameState
mergeLevelBuilder builder state = addEntitiesToGame entities state
  where
    entities = mkEntitiesFrom builder

mkEntitiesFrom :: LevelBuilder -> [Entity]
mkEntitiesFrom (LevelBuilder builder) = fmap mkEntityFrom (toList builder)

mkEntityFrom :: (Coord, Tile) -> Entity
mkEntityFrom (coord,Wall) = mkWall coord
mkEntityFrom (coord,Floor) = mkFloor coord

mkLevel :: GameState
mkLevel = mergeLevelBuilder level $ mkGameState (Coord 15 10)
  where
    level = mkRoom (Bounds (Coord 0 0) (Coord 30 20)) <> mkTiles Wall [Coord 5 5, Coord 11 9]
