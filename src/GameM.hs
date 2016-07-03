{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameM where

import Control.Category
import Control.Lens
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.IntMap.Strict as IntMap


import Prelude hiding (Either(..), id, (.))
import Coord
import Entity
import GameState
import GameMap


newtype GameM a = GameM
    { runGame :: (Reader.ReaderT GameState IO) a
    } deriving (Functor,Applicative,Random.MonadRandom,Monad,MonadReader GameState,State.MonadIO)

getEntity :: EntityRef -> GameM (Maybe Entity)
getEntity ref = do
    state <- ask
    return $ IntMap.lookup ref (state ^. gameEntities)

traversableAt :: Coord -> GameM Bool
traversableAt coord = do
    state <- ask
    let gameMap = state ^. obstructionByCoord
    return $ (gameMap <!!> coord) ^. traversable

transparentAt :: Coord -> GameM Bool
transparentAt coord = do
    state <- ask
    let gameMap = state ^. obstructionByCoord
    return $ (gameMap <!!> coord) ^. transparent

opaqueAt :: Coord -> GameM Bool
opaqueAt coord = not <$> transparentAt coord

attackablesAt :: Coord -> GameM [Entity]
attackablesAt coord = do
    entities <- entitiesAt coord
    return $ filter isAttackable entities

isAttackableRef :: EntityRef -> GameM Bool
isAttackableRef ref = do
    entity <- getEntity ref
    return $ maybe False isAttackable entity

entityCanMoveBy :: Entity -> Coord -> GameM Bool
entityCanMoveBy e c = traversableAt (c + e ^. position)

getDeltaTowardsPlayer :: Entity -> GameM Coord
getDeltaTowardsPlayer entity = do
    state <- ask
    let playerPos = state ^. playerPosition
        entityPos = entity ^. position
    return $ coordSgn (playerPos - entityPos)

entitiesAt :: Coord -> GameM [Entity]
entitiesAt coord = do
    state <- ask
    return $ (state ^. entitiesByCoord) <!!> coord
