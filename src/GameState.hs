{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Debug.Trace
import Debug.Trace.Helpers
import Prelude hiding (Either(..), id, (.))
import Control.Applicative
import Control.Category
import GHC.Generics
import Control.Lens
import qualified Data.IntMap.Strict as IntMap
import Data.Default
import qualified Data.Dequeue as DQ
import Data.Maybe (fromJust, isJust, isNothing)
import Coord
import Entity
import ActorQueue
import GameMap


data LevelState = LevelState {
  _gameEntities :: IntMap.IntMap Entity,
  _bounding :: Bounds
} deriving (Show, Generic)

makeLenses ''LevelState

data GameState = GameState
    {  _currLevel :: LevelState
    , _actorQueue :: ActorQueue
    , _nextEntityRef :: Int
    , _entitiesByCoord :: Maybe EntityMap
    , _obstructionByCoord :: Maybe ObstructionMap
    , _visibleToPlayer :: Maybe VisibleMap
    } deriving (Show,Generic)

makeLenses ''GameState

mkGameState :: Coord -> GameState
mkGameState playerStart =
    GameState
    { _currLevel = level
    , _actorQueue = queue
    , _nextEntityRef = 2
    , _entitiesByCoord = Nothing
    , _obstructionByCoord = Nothing
    , _visibleToPlayer = Nothing
    }
  where
    level = LevelState {  _gameEntities = entities
                        , _bounding = (Bounds (Coord 0 0) (Coord 100 100)) -- FIXME
                      }
    player = (mkPlayer playerStart) & entityRef .~ 1
    entities = IntMap.singleton 1 player
    queue = DQ.fromList [1]

buildMaps :: LevelState -> (EntityMap, ObstructionMap)
buildMaps currLevel = (entityMap, obstructionMap) where
  entities = levelEntities currLevel
  bounds = currLevel ^. bounding
  entityMap = mkEntityMap bounds entities
  obstructionMap = mkObstructionMap entityMap

mkNewEntityRef :: GameState -> (EntityRef, GameState)
mkNewEntityRef state = (state ^. nextEntityRef, state & nextEntityRef +~ 1)

addEntityToCurrLevel :: Entity -> GameState -> GameState
addEntityToCurrLevel entity gameState =
    gameState' & (currLevel . gameEntities) %~ addEntity
               & actorQueue %~ addQueue
  where
    (ref,gameState') = mkNewEntityRef gameState
    entity' = entity & entityRef .~ ref
    addEntity ents = IntMap.insert ref entity' ents
    addQueue queue =
        if isJust $ entity' ^. actor
            then DQ.pushBack queue ref
            else queue

addEntitiesToCurrLevel :: [Entity] -> GameState -> GameState
addEntitiesToCurrLevel ents gameState = Prelude.foldr addEntityToCurrLevel gameState ents

instance Default GameState where
    def = mkGameState (Coord 1 1)

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = anon (error "unsafeFromJust: Nothing") (const False)

player :: Lens' GameState Entity
player = currLevel . gameEntities . (at 1) . unsafeFromJust

playerPosition :: Lens' GameState Coord
playerPosition = player . position

levelEntities :: LevelState -> [Entity]
levelEntities currLevel = IntMap.elems (currLevel ^. gameEntities)
