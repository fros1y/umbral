module AIStrategies where

import           Control.Applicative
import           Control.Category
import           Control.Concurrent
import           Control.Lens
import           Control.Monad        (when)
import qualified Control.Monad.Loops  as L
import qualified Control.Monad.Random as Random
import           Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State
import           Coord
import           Data.Default
import qualified Data.Dequeue         as DQ
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isJust, isNothing, listToMaybe)
import           Debug.Trace
import           Debug.Trace.Helpers
import           GHC.Generics
import           Prelude              hiding (Either (..), id, (.))
import           Symbol

import ActorQueue
import Entity
import Actions
import Effects
import GameState
import UI
import GameM
import Utils

runRandom :: Entity -> GameM ActionsByEntity
runRandom entity = do
  possibleMoves <- randomDeltas
  validMoves <- filterM (entityCanMoveBy entity) possibleMoves
  let validMove = listToMaybe validMoves
  return $ returnActionsFor entity (case validMove of
                                      Nothing -> [ActWait]
                                      (Just m) -> [ActMoveBy m])

runZombie :: Entity -> GameM ActionsByEntity
runZombie entity = do
  towardsPlayer <- getDeltaTowardsPlayer entity
  let possibleMoves = splitCoordDelta towardsPlayer
  validMoves <- filterM (entityCanMoveBy entity) possibleMoves
  let validMove = listToMaybe validMoves
  return $ returnActionsFor entity (case validMove of
                                      Nothing -> [ActWait]
                                      (Just m) -> [ActMoveBy m])
