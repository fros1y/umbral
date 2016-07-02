{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameM where

import Control.Category
import Control.Lens
import qualified Control.Monad.Random as Random
import Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array.IArray as IArray
import qualified Data.Array.IO as IOArray
import qualified FOV as FOV

import Prelude hiding (Either(..), id, (.))
import Coord
import Entity
import GameState

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
    return $ all isTraversable (entitiesAt coord $ allEntities state)

opaqueAt :: Coord -> GameM Bool
opaqueAt coord = do
    state <- ask
    return $ any isOpaque (entitiesAt coord $ allEntities state)

attackablesAt :: Coord -> GameM [Entity]
attackablesAt coord = do
    state <- ask
    return $ filter isAttackable $ entitiesAt coord $ allEntities state

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

initFOV :: IO FOV.Settings
initFOV = do
    settings <- FOV.newSettings
    FOV.setShape settings FOV.Circle
    FOV.setOpaqueApply settings True
    return settings

runFOV :: Coord -> [Coord] -> IO [Coord]
runFOV fromPos obstructions = do
    s <- initFOV
    visible <- IOArray.newArray ((0,0), (50,50)) False :: IO (IOArray.IOUArray (Int, Int) Bool)
    let lightGrid :: Int -> Int -> IO ()
        lightGrid x y = IOArray.writeArray visible (x, y) True

        checkOpaque :: Int -> Int -> IO Bool
        checkOpaque x y = return $ (fromPair (x, y)) `elem` obstructions

    FOV.circle s (toPair fromPos) 10 lightGrid checkOpaque
    visible' <- IOArray.freeze visible :: IO (IArray.Array (Int, Int) Bool)

    return $ fmap (\(c, _) -> fromPair c) $ filter (\(c, v) -> v) $ IArray.assocs visible'

getVisibleBy :: Entity -> GameM [Coord]
getVisibleBy from = do
    state <- ask
    let obstructions = fmap _position $ filter isOpaque (allEntities state)
        fromPos = from ^. position
    visible <- liftIO $ runFOV fromPos obstructions
    return visible
