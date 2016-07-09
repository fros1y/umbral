{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameEngine where

import Control.Category
import Control.Lens
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust)


import Prelude hiding (Either(..), id, (.))
import Coord
import Entity
import GameState
import GameMap


newtype GameEngine a = GameEngine
    { runGame :: (Reader.ReaderT GameState IO) a
    } deriving (Functor,Applicative,Random.MonadRandom,Monad,MonadReader GameState,State.MonadIO)

getEntity :: EntityRef -> GameEngine (Maybe Entity)
getEntity ref = do
    state <- ask
    return $ IntMap.lookup ref (state ^. (currLevel. gameEntities))

unsafeGetCachedMap :: GameState -> CachedMap
unsafeGetCachedMap gameState = fromJust cachedMap' where
    cachedMap' = gameState ^. (currLevel . cachedMap)

traversableAt :: Coord -> GameEngine Bool
traversableAt coord = do
    state <- ask
    let gameMap = unpackObstructionMap $ (unsafeGetCachedMap state) ^. obstructionMap
    return $ (gameMap <!> coord) ^. traversable

transparentAt :: Coord -> GameEngine Bool
transparentAt coord = do
    state <- ask
    let gameMap = unpackObstructionMap $ (unsafeGetCachedMap state) ^. obstructionMap
    return $ (gameMap <!> coord) ^. transparent

opaqueAt :: Coord -> GameEngine Bool
opaqueAt coord = not <$> transparentAt coord

attackablesAt :: Coord -> GameEngine [Entity]
attackablesAt coord = do
    entities <- entitiesAt coord
    return $ filter isAttackable entities

isAttackableRef :: EntityRef -> GameEngine Bool
isAttackableRef ref = do
    entity <- getEntity ref
    return $ maybe False isAttackable entity

entityCanMoveBy :: Entity -> Coord -> GameEngine Bool
entityCanMoveBy e c = traversableAt (c + e ^. position)

getDeltaTowardsPlayer :: Entity -> GameEngine Coord
getDeltaTowardsPlayer entity = do
    state <- ask
    let playerPos = state ^. playerPosition
        entityPos = entity ^. position
    return $ coordSgn (playerPos - entityPos)

entitiesAt :: Coord -> GameEngine [Entity]
entitiesAt coord = do
    state <- ask
    return $ (unpackEntityMap $ (unsafeGetCachedMap state) ^. entityMap) <!> coord
