module AIStrategies where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad (when)
import qualified Control.Monad.Loops as L
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Coord
import Data.Default
import qualified Data.Dequeue as DQ
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Debug.Trace
import Debug.Trace.Helpers
import GHC.Generics
import Prelude hiding (Either(..), id, (.))
import qualified System.Random.Shuffle as Random
import Actions
import ActorQueue
import Effects
import Entity
import GameEngine
import GameState
import Symbol
import UI
import Utils

type Proposer = GameEngine [Action]

type Chooser = [Action] -> GameEngine (Maybe Action)

runEntity :: Entity -> GameEngine ActionsByEntity
runEntity entity =
    case entityStrategy entity of
        Nothing -> return $ returnActionsFor entity []
        Just Random -> runRandom entity
        Just Zombie -> runZombie entity

runRandom :: Entity -> GameEngine ActionsByEntity
runRandom entity = runEntity' propose choose entity
  where
    propose = attacksBy entity +++ allMovesBy entity
    choose = pickRandomAction

runZombie :: Entity -> GameEngine ActionsByEntity
runZombie entity = runEntity' propose choose entity
  where
    propose = attacksBy entity +++ dumbMovesTowardPlayer entity
    choose = return <<< listToMaybe

runEntity' :: Proposer -> Chooser -> Entity -> GameEngine ActionsByEntity
runEntity' propose choose entity = do
    possibleActions <- propose
    chosenAct <- choose possibleActions
    return $
        returnActionsFor
            entity
            (case chosenAct of
                 Nothing -> [ActWait]
                 (Just m) -> [m])

attacksAt :: Coord -> GameEngine [Action]
attacksAt coord = do
    attackables <- attackablesAt coord
    return $ fmap (ActAttack <<< _entityRef) attackables

dumbMovesTowardPlayer :: Entity -> GameEngine [Action]
dumbMovesTowardPlayer entity = do
    towardsPlayer <- liftA splitCoordDelta $ getDeltaTowardsPlayer entity
    validMoves <- filterM (entityCanMoveBy entity) towardsPlayer
    return $ fmap ActMoveBy validMoves

allMovesBy :: Entity -> GameEngine [Action]
allMovesBy entity = do
    validMoves <-
        filterM
            (entityCanMoveBy entity)
            [ (Coord xs ys)
            | xs <- [-1, 0, 1]
            , ys <- [-1, 0, 1] ]
    return $ fmap ActMoveBy validMoves

pickRandomAction
    :: (Random.MonadRandom m)
    => [Action] -> m (Maybe Action)
pickRandomAction actions = do
    randomActions <- Random.shuffleM actions
    return $ listToMaybe randomActions

attacksBy :: Entity -> GameEngine [Action]
attacksBy entity = do
    let coords =
            fmap
                (+ entity ^. position)
                [ (Coord xs ys)
                | xs <- [-1, 0, 1]
                , ys <- [-1, 0, 1]
                , not (xs == 0 && ys == 0) ]
    attacks <- mapM attacksAt coords
    return $ mconcat attacks
