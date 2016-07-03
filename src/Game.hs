{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad (when)
import FOV as FOV
import qualified Control.Monad.Loops as L
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import Data.Default
import qualified Data.Dequeue as DQ
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Debug.Trace
import Debug.Trace.Helpers
import GHC.Generics
import Prelude hiding (Either(..), id, (.))
import Actions
import ActorQueue
import AIStrategies
import Coord
import Effects
import Entity
import GameM
import GameState
import Serialize
import Symbol
import UI
import Utils
import GameMap
import Data.Array

data GameCommand
    = C_NOP
    | C_Quit
    | C_Save
    | C_Load
    deriving (Eq,Generic,Show)

saveGame :: GameState -> IO ()
saveGame state = do
    let filename = "out.umbral"
    writeFile filename <<< show $ Aeson.encode state

loadGame :: IO (Maybe GameState)
loadGame = do
    let filename = "out.umbral"
    fileContents <- readFile filename
    return $ Aeson.decode (read fileContents)

prepareMapsForState :: GameState -> GameState
prepareMapsForState state = state { _entitiesByCoord = Just entityMap,
                                    _obstructionByCoord = Just obstructionMap,
                                    _visibleToPlayer = Just playerVisible
                                } where
    (entityMap, obstructionMap) = buildMaps (state ^. currLevel)
    playerVisible = mkVisibleMap (state ^. player) obstructionMap

gameLoop :: DisplayContext -> GameState -> IO ()
gameLoop display gameState = do
    (state',gameCommand) <-
        Reader.runReaderT (runGame (gameStepM display)) (prepareMapsForState gameState)
    case gameCommand of
        Nothing -> gameLoop display state'
        Just C_NOP -> gameLoop display state'
        Just C_Save -> do
            saveGame state'
            gameLoop display state'
        Just C_Load -> do
            loadState <- loadGame
            case loadState of
                Nothing ->
                    traceMsgM "Error loading game." $ gameLoop display state'
                (Just loadState') -> gameLoop display loadState'
        Just C_Quit -> return ()


gameStepM :: DisplayContext -> GameM (GameState, Maybe GameCommand)
gameStepM display = do
    state <- ask
    liftIO $ render display state (fromJust (state ^. visibleToPlayer))
    entityToRun <- firstInQueue
    if stillActive entityToRun
        then entityStepM display (fromJust entityToRun)
        else do
            state' <- rotateAndStep entityToRun
            return (state', Nothing)

entityStepM :: DisplayContext -> Entity -> GameM (GameState, Maybe GameCommand)
entityStepM display entityToRun = do
    state <- ask
    (actions,command) <-
        if isPlayerEntity entityToRun
            then getPlayerActions display entityToRun
            else do
                acts <- runEntity entityToRun
                return (acts, Nothing)
    if (isJust command)
        then return (state, command)
        else do
            effects <- applyActions (traceMsg "actions: " actions)
            state' <- applyEffectsToEntities (traceMsg "effects: " effects) -- return mutated gameState
            return (state', command)

rotateAndStep :: Maybe Entity -> GameM GameState
rotateAndStep e = do
    case e of
        Nothing       -- this happens if an entity has been destroyed (died)
         -> do
            gameState <- ask
            return $ gameState & actorQueue %~ dropFront
        (Just e') -> do
            let recover = returnEffectsForRef (e' ^. entityRef) [EffRecoverAP]
            gameState' <- applyEffectsToEntities recover
            return $ gameState' & actorQueue %~ rotate

firstInQueue :: GameM (Maybe Entity)
firstInQueue = do
    state <- ask
    let ref = (DQ.first (state ^. actorQueue)) :: Maybe EntityRef
    case ref of
        Nothing -> return Nothing
        Just r -> getEntity r

isAttack :: Coord -> GameM (Maybe EntityRef)
isAttack coord = do
    attackables <- attackablesAt coord
    return $ _entityRef <$> listToMaybe attackables

getPlayerActions :: DisplayContext
                 -> Entity
                 -> GameM (ActionsByEntity, Maybe GameCommand)
getPlayerActions display player = do
    let playerActs = returnActionsFor player
    input <- liftIO $ getPlayerCommand display
    case input of
        Nothing -> return (playerActs [], Just C_NOP)
        Just Pass -> return (playerActs [ActWait], Nothing)
        Just Quit -> return (playerActs [], Just C_Quit)
        Just Save -> return (playerActs [], Just C_Save)
        Just Load -> return (playerActs [], Just C_Load)
        Just (Go d) -> do
            let delta = fromDirection d
            attacked <- isAttack (player ^. position + delta)
            case attacked of
                Nothing ->
                    return (playerActs [ActMoveBy $ fromDirection d], Nothing)
                (Just ref) -> return (playerActs [ActAttack ref], Nothing)
        _ -> return (returnActionsFor player [], Nothing)
