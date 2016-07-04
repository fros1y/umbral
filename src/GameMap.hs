module GameMap where

import Data.Array
import Prelude hiding (Either(..), id, (.))
import Data.Monoid
import Control.Lens
import Data.Maybe
import System.IO.Unsafe
import Coord
import Entity
import Control.Monad
import qualified Data.Array.IO as IOArray
import qualified Data.Array.Unsafe as Unsafe
import qualified TCOD as TCOD


type CoordIndex = (Int, Int)

type GameMap a = Array CoordIndex a
type EntityMap = GameMap [Entity]
type ObstructionMap = GameMap Obstruction
type VisibleMap = GameMap Bool

mkEntityMap :: Bounds -> [Entity] -> EntityMap
mkEntityMap b entities = accumArray (<>) [] (boundsToPair b) placedEntities where
  placedEntities = fmap (\e -> (toPair $ e ^. position, [e])) entities

mkObstructionMap :: EntityMap -> ObstructionMap
mkObstructionMap entityMap = (determineObstructions <$> entityMap) where
  determineObstructions :: [Entity] -> Obstruction
  determineObstructions entities = fromMaybe (Obstruction True True) (checkEntities entities)
  checkEntities :: [Entity] -> Maybe Obstruction
  checkEntities entities = mconcat $ (\e -> e ^. obstruction) <$> entities

traversableAt' :: ObstructionMap -> Coord -> Bool
traversableAt' gameMap coord = (gameMap <!> coord) ^. traversable

transparentAt' :: ObstructionMap -> Coord -> Bool
transparentAt' gameMap coord = (gameMap <!> coord) ^. transparent

mkVisibleMap :: Entity -> ObstructionMap -> VisibleMap
mkVisibleMap fromEntity obstructionMap = visibleMap where
  visibleMap = unsafePerformIO visibleMap'
  visibleMap' = runFOV (fromEntity ^. position) obstructionMap

buildTCODMap :: ObstructionMap -> IO TCOD.TCODMap
buildTCODMap obstructionMap = do
  let (_, (xSize, ySize)) = bounds obstructionMap
  tcodMap <- TCOD.newMap xSize ySize
  forM_ (Data.Array.indices obstructionMap) $ \(x, y) -> do
      let transp = (obstructionMap ! (x,y)) ^. transparent
          walkable = (obstructionMap ! (x,y)) ^. traversable
      TCOD.setGrid tcodMap x y transp walkable
  return tcodMap

runFOV :: Coord -> ObstructionMap -> IO VisibleMap
runFOV fromPos obstructionMap = do
  let (xPos, yPos) = toPair fromPos
  tcodMap <- buildTCODMap obstructionMap
  TCOD.computeFOVFrom tcodMap xPos yPos 0 True

  visible <- IOArray.newArray (bounds obstructionMap) False :: IO (IOArray.IOUArray CoordIndex Bool)
  forM_ (Data.Array.indices obstructionMap) $ \(x, y) -> do
    inField <- TCOD.inFOV tcodMap x y
    when inField $ IOArray.writeArray visible (x, y) True

  visible' <- Unsafe.unsafeFreeze visible :: IO VisibleMap
  return visible'

mapLookup' :: Maybe (GameMap a) -> Coord -> a
mapLookup' (Just gameMap) coord = mapLookup gameMap coord
mapLookup' Nothing _ = undefined

mapLookup :: GameMap a -> Coord -> a
mapLookup gameMap coord = (gameMap ! toPair coord)

(<!>) :: GameMap a -> Coord -> a
(<!>) = mapLookup

(<!!>) :: Maybe (GameMap a) -> Coord -> a
(<!!>) = mapLookup'
