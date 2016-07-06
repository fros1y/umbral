{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GameMap where

import GHC.Generics
import Control.Lens
import Data.Array
import Prelude hiding (Either(..), id, (.))
import Data.Monoid
import Control.Lens
import Data.Maybe
import qualified Data.Semigroup as Semigroup

import System.IO.Unsafe
import Coord
import Entity
import Control.Monad
import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as Unsafe
import qualified TCOD as TCOD
import Lighting

type CoordIndex = (Int, Int)

type GameMap a = Array CoordIndex a
newtype EntityMap = EntityMap {unpackEntityMap :: (GameMap [Entity])} deriving (Generic)
newtype ObstructionMap = ObstructionMap {unpackObstructionMap :: (GameMap Obstruction)} deriving (Generic)
newtype VisibleMap = VisibleMap {unpackVisibleMap :: (GameMap Bool)} deriving (Generic)
newtype LightMap = LightMap {unpackLightMap :: (GameMap LightLevel)} deriving (Generic)


instance Semigroup.Semigroup LightMap where
  (<>) (LightMap m1) (LightMap m2) = LightMap $ accumArray (<>) mempty (bounds m1) ((assocs m1) ++ (assocs m2))

instance SemiGroup.SemiGroup ObstructionMap where
  (<>) (ObstructionMap m1) (ObstructionMap m2) = ObstructionMap $ accumArray (<>) mempty (bounds m1) ((assocs m1) ++ (assocs m2))


data CachedMap = CachedMap {
  _entityMap :: EntityMap,
  _obstructionMap :: ObstructionMap,
  _tcodMap :: TCOD.TCODMap,
  _lightMap :: LightMap
} deriving (Generic)

instance Show CachedMap where
  show cachedMap = "CachedMap xxx"

makeLenses ''CachedMap

mkEntityMap :: Bounds -> [Entity] -> EntityMap
mkEntityMap b entities = EntityMap $ accumArray (<>) [] (boundsToPair b) placedEntities where
  placedEntities = fmap (\e -> (toPair $ e ^. position, [e])) entities

mkObstructionMap :: EntityMap -> ObstructionMap
mkObstructionMap (EntityMap entityMap) = ObstructionMap (determineObstructions <$> entityMap) where
  determineObstructions :: [Entity] -> Obstruction
  determineObstructions entities = fromMaybe (Obstruction True True) (checkEntities entities)
  checkEntities :: [Entity] -> Maybe Obstruction
  checkEntities entities = mconcat $ (\e -> e ^. obstruction) <$> entities

traversableAt' :: ObstructionMap -> Coord -> Bool
traversableAt' (ObstructionMap gameMap) coord = (gameMap <!> coord) ^. traversable

transparentAt' :: ObstructionMap -> Coord -> Bool
transparentAt' (ObstructionMap gameMap) coord = (gameMap <!> coord) ^. transparent

mkVisibleMap :: Entity -> CachedMap -> VisibleMap
mkVisibleMap fromEntity cachedMap = runFOV (fromEntity ^. position) size' tcodMap' where
  size' = bounds $ unpackObstructionMap (cachedMap ^. obstructionMap)
  tcodMap' = cachedMap ^. tcodMap

{- NOINLINE buildTCODMap -}
mkTCODMap :: ObstructionMap -> TCOD.TCODMap
mkTCODMap (ObstructionMap obstructionMap) = unsafePerformIO $ do
  let (_, (xSize, ySize)) = bounds obstructionMap
  tcodMap <- TCOD.newMap xSize ySize
  forM_ (Data.Array.indices obstructionMap) $ \(x, y) -> do
      let transp = (obstructionMap ! (x,y)) ^. transparent
          walkable = (obstructionMap ! (x,y)) ^. traversable
      TCOD.setGrid tcodMap x y transp walkable
  return tcodMap

indexList :: (CoordIndex, CoordIndex) -> [CoordIndex]
indexList ((lx, ly), (ux, uy)) = do
  x <- [lx .. ux]
  y <- [ly .. uy]
  return (x, y)

{- NOINLINE runFOV -}
runFOV :: Coord -> (CoordIndex, CoordIndex) -> TCOD.TCODMap -> VisibleMap
runFOV fromPos mapBounds tcodMap = VisibleMap $ unsafePerformIO $ do
  let (xPos, yPos) = toPair fromPos
  TCOD.computeFOVFrom tcodMap xPos yPos 0 True

  visible <- IOArray.newArray mapBounds False :: IO (IOArray.IOUArray CoordIndex Bool)
  forM_ (indexList mapBounds) $ \(x, y) -> do
    inField <- TCOD.inFOV tcodMap x y
    when inField $ IOArray.writeArray visible (x, y) True

  visible' <- Unsafe.unsafeFreeze visible
  return visible'




{- NOINLINE mkLightMapFromEntitySource -}
mkLightMapFromEntitySource :: Coord -> LightSource -> VisibleMap -> LightMap
mkLightMapFromEntitySource pos source (VisibleMap visibleMap) = LightMap $ unsafePerformIO $ do
  let posPair = toPair pos
  lighting <- IOArray.newArray (bounds visibleMap) mempty :: IO (IOArray.IOArray CoordIndex LightLevel)
  forM_ (indexList $ bounds visibleMap) $ \(x, y) -> do
    when (visibleMap ! (x,y)) $
      IOArray.writeArray lighting (x, y) $
        castLight source (pairDistance posPair (x,y))
  lighting' <- Unsafe.unsafeFreeze lighting
  return lighting'

mapLookup' :: Maybe (GameMap a) -> Coord -> a
mapLookup' (Just gameMap) coord = mapLookup gameMap coord
mapLookup' Nothing _ = undefined

mapLookup :: GameMap a -> Coord -> a
mapLookup gameMap coord = (gameMap ! toPair coord)

(<!>) :: GameMap a -> Coord -> a
(<!>) = mapLookup

(<!!>) :: Maybe (GameMap a) -> Coord -> a
(<!!>) = mapLookup'
