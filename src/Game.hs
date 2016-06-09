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
import AIStrategies

data GameCommand =  C_Quit |
                    C_Save |
                    C_Load deriving (Eq, Generic, Show)

gameLoop :: DisplayContext -> GameState -> IO ()
gameLoop display gameState = do
  (state', gameCommand) <- Reader.runReaderT (runGame (gameStepM display)) gameState
  case gameCommand of
    Nothing -> gameLoop display state'
    Just C_Quit -> return ()

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

firstInQueue :: GameM (Maybe Entity)
firstInQueue = do
  state <- ask
  let ref = (DQ.first (state ^. actorQueue)) :: Maybe EntityRef
  case ref of Nothing -> return Nothing
              Just r -> getEntity r

getPlayerActions :: DisplayContext -> Entity -> GameM (ActionsByEntity, Maybe GameCommand)
getPlayerActions display player = do
  input <- liftIO $ getPlayerCommand display
  let (act, command) = case input of
                            Nothing -> ([], Nothing)
                            Just (Go d) -> ([ActMoveBy $ fromDirection d], Nothing)
                            Just Quit -> ([], Just C_Quit)
                            _ -> ([], Nothing)
  return $ (returnActionsFor player act, command)
