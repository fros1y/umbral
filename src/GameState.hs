{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Prelude hiding (Either(..), id, (.))
import Control.Category
import GHC.Generics
import Control.Lens
import qualified Data.IntMap.Strict as IntMap
import Data.Default
import qualified Data.Dequeue as DQ
import Data.Maybe (isJust)
import Coord
import Entity
import ActorQueue
import GameMap


data LevelState = LevelState {
    _gameEntities :: IntMap.IntMap Entity
  , _bounding :: Bounds
  , _cachedMap :: Maybe CachedMap
} deriving (Show, Generic)

makeLenses ''LevelState

data GameState = GameState
    {  _currLevel :: LevelState
    , _actorQueue :: ActorQueue
    , _nextEntityRef :: Int
    } deriving (Show,Generic)

makeLenses ''GameState

mkGameState :: Coord -> GameState
mkGameState playerStart =
    GameState
    { _currLevel = level
    , _actorQueue = queue
    , _nextEntityRef = 2
    }
  where
    level = LevelState {  _gameEntities = entities
                        , _bounding = (Bounds (Coord 0 0) (Coord 100 100)) -- FIXME
                        , _cachedMap = Nothing
                      }
    playerE = (mkPlayer playerStart) & entityRef .~ 1
    entities = IntMap.singleton 1 playerE
    queue = DQ.fromList [1]

buildMaps :: LevelState -> LevelState
buildMaps level = level {_cachedMap = Just cachedMap'} where
  cachedMap' = buildCachedMap level

buildCachedMap :: LevelState -> CachedMap
buildCachedMap levelState = CachedMap entityMap obstructionMap tcodMap lightMap where
  levelBounds = (levelState ^. bounding)
  entityMap = mkEntityMap levelBounds (levelEntities levelState)
  obstructionMap = mkObstructionMap entityMap
  tcodMap = mkTCODMap obstructionMap
  lightMap = mkLightMap (levelLightSources levelState) (boundsToPair levelBounds) tcodMap

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
levelEntities level = IntMap.elems (level ^. gameEntities)

levelLightSources :: LevelState -> [Entity]
levelLightSources level = filter isLightSource $ levelEntities level
