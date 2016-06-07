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
import           Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State
import           Coord
import           Data.Default
import qualified Data.Dequeue         as DQ
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isJust, isNothing)
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
gameLoop display gameState = L.iterateM_ gameStep gameState where
  gameStep gameState = Reader.runReaderT (runGame (gameStepM display)) gameState

gameStepM :: DisplayContext -> GameM GameState
gameStepM display = do
  state <- ask
  liftIO $ render display state
  entityToRun <- firstInQueue
  if stillActive entityToRun
    then entityStepM display (fromJust entityToRun)
    else rotateAndStep (fromJust entityToRun)

entityStepM :: DisplayContext -> Entity -> GameM GameState
entityStepM display entityToRun = do
  actions <- if isPlayerEntity entityToRun
            then getPlayerActions display entityToRun
            else runEntity entityToRun
  effects <- applyActions actions
  applyEffectsToEntities effects -- return mutated gameState

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
applyActions actionsByEntity@(ref, actions) = do
  validActions <- validateActions actionsByEntity
  effects <- mapM (applyAction ref) validActions
  let cost = returnEffectsForRef ref [EffSpendAP $ (sum <<< (fmap determineActionCost)) validActions]
  return $ mconcat (cost:effects)

------
getPlayerActions :: DisplayContext -> Entity -> GameM ActionsByEntity
getPlayerActions display player = do
  command <- liftIO $ getPlayerCommand display
  let act = case command of Nothing -> []
                            Just (Go d) -> [ActMoveBy $ fromDirection d]
                            _ -> trace "Unknown action" []
  return $ returnActionsFor player act

-----

getRandomDirection :: (Random.MonadRandom m) => m Direction
getRandomDirection = Random.uniform [Coord.Left, Coord.Right, Coord.Down, Coord.Up]

------

runEntity :: Entity -> GameM ActionsByEntity
runEntity entity = do
  randomDirection <- getRandomDirection
  return $ returnActionsFor entity [ActMoveBy $ fromDirection randomDirection]

-------

validateActions :: ActionsByEntity -> GameM [Action]
validateActions (ref, actions) = return actions

-------

applyAction :: EntityRef -> Action -> GameM EffectsToEntities
applyAction ref ActPlayerTurnDone   = return $ returnEffectsForAll [EffRecoverAP]
applyAction ref (ActMoveBy delta)   = do
  e <- fromJust <$> getEntity ref
  return (returnEffectsForRef ref [EffMoveTo $ (e ^. position) + delta])
applyAction ref _                   = return mempty

-------

applyEffectsToEntities :: EffectsToEntities -> GameM GameState
applyEffectsToEntities effects = do
  gameState <- ask
  let gameEntities' = IntMap.mergeWithKey applyEffects (const IntMap.empty) id (getMap effects) (gameState ^. gameEntities)
      gameEntities'' = applyBroadcastEffects (IntMap.lookup (-1) (getMap effects)) gameEntities'
      gameState'    = gameState
                    & gameEntities .~ gameEntities''
  return $ gameState'
