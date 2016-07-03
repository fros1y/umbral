module GameMap where

import Data.Array
import Prelude hiding (Either(..), id, (.))
import Data.Monoid
import Control.Lens
import Data.Maybe
import System.IO.Unsafe
import Coord
import Entity
import qualified Data.Array.IO as IOArray
import qualified FOV as FOV

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

initFOV :: IO FOV.Settings
initFOV = do
    settings <- FOV.newSettings
    FOV.setShape settings FOV.Circle
    FOV.setOpaqueApply settings True
    return settings

runFOV :: Coord -> ObstructionMap -> IO VisibleMap
runFOV fromPos obstructionMap = do
  s <- initFOV
  visible <- IOArray.newArray (bounds obstructionMap) False :: IO (IOArray.IOUArray CoordIndex Bool)
  let makeVisible :: Int -> Int -> IO ()
      makeVisible x y = IOArray.writeArray visible (x, y) True
      checkOpaque :: Int -> Int -> IO Bool
      checkOpaque x y = return $ not (transparentAt' obstructionMap (fromPair (x, y)))
  FOV.circle s (toPair fromPos) 999 makeVisible checkOpaque
  visible' <- IOArray.freeze visible :: IO VisibleMap
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
