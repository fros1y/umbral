{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameM where

import qualified Control.Monad.Random as Random
import qualified Control.Monad.State  as State
import           Control.Monad.Reader as Reader
import           Control.Lens
import           Control.Category
import           Prelude              hiding (Either (..), id, (.))
import qualified Data.IntMap.Strict   as IntMap

import GameState
import Entity
import Coord

newtype GameM a = GameM {
  runGame :: (Reader.ReaderT GameState IO) a
} deriving (Functor, Applicative, Random.MonadRandom, Monad, MonadReader GameState, State.MonadIO)

getEntity :: EntityRef -> GameM (Maybe Entity)
getEntity ref = do
  state <- ask
  return $ IntMap.lookup ref (state ^. gameEntities)

traversableAt :: Coord -> GameM Bool
traversableAt coord = do
  state <- ask
  return $ not (any isTraversable . entitiesAt coord $ allEntities state)

attackablesAt :: Coord -> GameM [Entity]
attackablesAt coord = do
  state <- ask
  return $ filter isAttackable $ entitiesAt coord $ allEntities state

isAttackableRef :: EntityRef -> GameM Bool
isAttackableRef ref = do
  entity <- getEntity ref
  return $ maybe False isAttackable entity
--
-- attackable :: EntityRef -> GameM Bool
-- attackable ref = do
--   e <- getEntity ref
--   return $ maybe False isAttackable e

entityCanMoveBy :: Entity -> Coord -> GameM Bool
entityCanMoveBy e c = traversableAt (c + e ^. position)

getDeltaTowardsPlayer :: Entity -> GameM Coord
getDeltaTowardsPlayer entity = do
  state <- ask
  let playerPos = state ^. playerPosition
      entityPos = entity ^. position
  return $ coordSgn (playerPos - entityPos)
