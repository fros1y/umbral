{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Game where

import           Control.Applicative
import           Control.Category
import           Control.Concurrent
import           Control.Lens
import           Control.Monad        (when)
import qualified Control.Monad.Loops  as L
import qualified Control.Monad.Random as Random
import qualified System.Random.Shuffle as Random
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

newtype GameM a = GameM {
  runGame :: (Reader.ReaderT GameState IO) a
} deriving (Functor, Applicative, Random.MonadRandom, Monad, MonadReader GameState, State.MonadIO)

traceMsgM :: (Monad m, Show r) => [Char] -> m r -> m r
traceMsgM a = State.liftM $ traceMsg a

gameLoop :: DisplayContext -> GameState -> IO ()
gameLoop display gameState = do
  (state', gameCommand) <- Reader.runReaderT (runGame (gameStepM display)) gameState
  case gameCommand of
    Nothing -> gameLoop display state'
    Just C_Quit -> return ()

gameRunning :: GameState -> Bool
gameRunning state = state ^. continueRunning

data GameCommand =  C_Quit |
                    C_Save |
                    C_Load deriving (Eq, Generic, Show)

gameStepM :: DisplayContext -> GameM (GameState, Maybe GameCommand)
gameStepM display = do
  state <- ask
  liftIO $ render display state
  entityToRun <- firstInQueue
  if stillActive entityToRun
    then entityStepM display (fromJust entityToRun)
    else do
      state' <- rotateAndStep (fromJust entityToRun)
      return (state', Nothing)

entityStepM :: DisplayContext -> Entity -> GameM (GameState, Maybe GameCommand)
entityStepM display entityToRun = do
  (actions, command) <- if isPlayerEntity entityToRun
                        then getPlayerActions display entityToRun
                        else do
                          acts <- runEntity entityToRun
                          return (acts, Nothing)
  effects <- applyActions actions
  state' <- applyEffectsToEntities effects -- return mutated gameState
  return (state', command)

rotateAndStep :: Entity -> GameM GameState
rotateAndStep exhaustedEntity = do
  let recover = returnEffectsForRef (exhaustedEntity ^. entityRef) [EffRecoverAP]
  gameState' <- applyEffectsToEntities recover
  return $ gameState' & actorQueue %~ rotate

getEntity :: EntityRef -> GameM (Maybe Entity)
getEntity ref = do
  state <- ask
  return $ IntMap.lookup ref (state ^. gameEntities)

firstInQueue :: GameM (Maybe Entity)
firstInQueue = do
  state <- ask
  let ref = (DQ.first (state ^. actorQueue)) :: Maybe EntityRef
  case ref of Nothing -> return Nothing
              Just r -> getEntity r

getEntityToRun :: GameM (Maybe Entity)
getEntityToRun = do
  candidate <- firstInQueue
  if stillActive candidate
    then return candidate
    else return Nothing

applyActions :: ActionsByEntity -> GameM EffectsToEntities
applyActions (_, []) = return mempty
applyActions actionsByEntity@(ref, actions) = do
  validActions <- validateActions actionsByEntity
  effects <- mapM (applyAction ref) validActions
  let cost = returnEffectsForRef ref [EffSpendAP $ (sum <<< (fmap determineActionCost)) validActions]
  return $ mconcat (cost:effects)

------
getPlayerActions :: DisplayContext -> Entity -> GameM (ActionsByEntity, Maybe GameCommand)
getPlayerActions display player = do
  input <- liftIO $ getPlayerCommand display
  let (act, command) = case input of
                            Nothing -> ([], Nothing)
                            Just (Go d) -> ([ActMoveBy $ fromDirection d], Nothing)
                            Just Quit -> ([], Just C_Quit)
                            _ -> ([], Nothing)
  return $ (returnActionsFor player act, command)

-----

getRandomDirection :: (Random.MonadRandom m) => m Direction
getRandomDirection = Random.uniform [Coord.Left, Coord.Right, Coord.Down, Coord.Up]

randomDeltas :: (Random.MonadRandom m) => m [Coord]
randomDeltas = Random.shuffleM [(Coord xs ys) | xs <- [-1, 0, 1],
                                               ys <- [-1, 0, 1]]

getDeltaTowardsPlayer :: Entity -> GameM Coord
getDeltaTowardsPlayer entity = do
  state <- ask
  let playerPos = state ^. playerPosition
      entityPos = entity ^. position
  return $ coordSgn (playerPos - entityPos)

splitCoordDelta :: Coord -> [Coord]
splitCoordDelta (Coord x y) = [(Coord xs ys) |  xs <- [x, 0],
                                                ys <- [y, 0]]
------

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

runEntity :: Entity -> GameM ActionsByEntity
runEntity entity = case entityStrategy entity of
  Nothing -> return $ returnActionsFor entity []
  Just Random -> runRandom entity
  Just Zombie -> runZombie entity
-------

validateActions :: ActionsByEntity -> GameM [Action]
validateActions (ref, actions) = do
  e <- getEntity ref
  case e of
    Nothing -> return [] -- a non-existent entity can do nothing
    (Just e') -> filterM (validActionBy e') actions

validActionBy :: Entity -> Action -> GameM Bool
validActionBy e (ActMoveBy delta) = traversableAt $ (e ^. position) + delta
validActionBy _ _ = return True

traversableAt :: Coord -> GameM Bool
traversableAt coord = do
  state <- ask
  return $ not (any isTraversable . entitiesAt coord $ allEntities state)

entityCanMoveBy :: Entity -> Coord -> GameM Bool
entityCanMoveBy e c = traversableAt (c + e ^. position)
-------

applyAction :: EntityRef -> Action -> GameM EffectsToEntities
-- applyAction ref ActPlayerTurnDone   = return $ returnEffectsForAll [EffRecoverAP]
applyAction ref ActWait             = return $ returnEffectsForRef ref [EffPass]
applyAction ref (ActMoveBy delta)   = do
  e <- fromJust <$> getEntity ref
  return (returnEffectsForRef ref [EffMoveTo $ (e ^. position) + delta])
applyAction ref _                   = return mempty

-------

applyEffectsToEntities :: EffectsToEntities -> GameM GameState
applyEffectsToEntities effects = do
  gameState <- ask
  let gameEntities' = IntMap.mergeWithKey applyEffects (const IntMap.empty) id (getMap effects) (gameState ^. gameEntities)
      gameState'    = gameState
                    & gameEntities .~ gameEntities'
  return $ gameState'
