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

data GameState = GameState
    { _gameEntities :: IntMap.IntMap Entity
    , _actorQueue :: ActorQueue
    , _nextEntityRef :: Int
    } deriving (Show,Generic)

makeLenses ''GameState

mkGameState :: Coord -> GameState
mkGameState playerStart =
    GameState
    { _gameEntities = entities
    , _actorQueue = queue
    , _nextEntityRef = 2
    }
  where
    player = (mkPlayer playerStart) & entityRef .~ 1
    entities = IntMap.singleton 1 player
    queue = DQ.fromList [1]

mkNewEntityRef :: GameState -> (EntityRef, GameState)
mkNewEntityRef state = (state ^. nextEntityRef, state & nextEntityRef +~ 1)

addEntityToGame :: Entity -> GameState -> GameState
addEntityToGame entity gameState =
    gameState' & gameEntities %~ addEntity & actorQueue %~ addQueue
  where
    (ref,gameState') = mkNewEntityRef gameState
    entity' = entity & entityRef .~ ref
    addEntity ents = IntMap.insert ref entity' ents
    addQueue queue =
        if isJust $ entity' ^. actor
            then DQ.pushBack queue ref
            else queue

addEntitiesToGame :: [Entity] -> GameState -> GameState
addEntitiesToGame ents gameState = Prelude.foldr addEntityToGame gameState ents

instance Default GameState where
    def = mkGameState (Coord 0 0)

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = anon (error "unsafeFromJust: Nothing") (const False)

player :: Lens' GameState Entity
player = gameEntities . (at 1) . unsafeFromJust

playerPosition :: Lens' GameState Coord
playerPosition = player . position

allEntities :: GameState -> [Entity]
allEntities state = IntMap.elems (state ^. gameEntities)
