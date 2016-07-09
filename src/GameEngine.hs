{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

module GameEngine where

import Control.Category
import Control.Lens
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust, mapMaybe)


import Prelude hiding (Either(..), id, (.))
import Coord
import Entity
import GameState
import GameMap
import UI
import Lighting
import Symbol


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

render :: DisplayContext -> GameState -> VisibleMap -> IO ()
render display state visibleMap = let ?context = display in do
    clearWindow display
    centerViewOn (state ^. playerPosition)
    let visible e = ((unpackVisibleMap visibleMap) <!> (e ^. position)) || (isPlayerEntity e)
        visibleEntities = filter visible (levelEntities (state ^. currLevel))
        litEntities = lightEntities visibleEntities (state ^. currLevel)
    mapM_ putEntity $ litEntities
    drawDisplay display

lightEntities :: [Entity] -> LevelState -> [Entity]
lightEntities entities lstate = mapMaybe lightEntity entities where
  cache = lstate ^. (cachedMap . unsafeFromJust)
  lightingMap = unpackLightMap $ cache ^. lightMap
  lightEntity e = if aboveThreshold 0.25 ll
                  then Just $ e & (symbol . baseColor) %~ (apparentColor ll)
                  else Nothing
              where
                ll = lightingMap <!> (e ^. position)
