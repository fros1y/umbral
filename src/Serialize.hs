{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serialize where

import Data.Aeson
import Data.Dequeue
import Coord
import Entity
import Symbol
import GameState
import Lighting
import qualified Color as Color

instance ToJSON Coord

instance FromJSON Coord

instance ToJSON Bounds

instance FromJSON Bounds

instance ToJSON Color.Color
instance FromJSON Color.Color

instance (Show a) => ToJSON (Data.Dequeue.BankersDequeue a) where
    toJSON = toJSON . show

instance (Read a) => FromJSON (Data.Dequeue.BankersDequeue a) where
    parseJSON = fmap read . parseJSON

instance ToJSON Symbol

instance FromJSON Symbol

instance ToJSON Health

instance FromJSON Health

instance ToJSON Actor

instance FromJSON Actor

instance ToJSON Strategy

instance FromJSON Strategy

instance ToJSON Obstruction

instance FromJSON Obstruction

instance ToJSON Entity

instance FromJSON Entity

instance ToJSON LevelState where
    toJSON LevelState{..} = object [
        "gameEntities" .= _gameEntities,
        "bounding" .= _bounding
        ]

instance FromJSON LevelState where
    parseJSON = withObject "levelstate" $ \o -> do
        _gameEntities <- o .: "gameEntities"
        _bounding <- o .: "bounding"
        _cachedMap <- return Nothing
        return LevelState {..}

instance ToJSON LightSource
instance FromJSON LightSource

instance ToJSON GameState
instance FromJSON GameState
