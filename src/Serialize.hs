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

instance ToJSON LevelState
instance FromJSON LevelState

instance ToJSON LightSource
instance FromJSON LightSource

instance ToJSON GameState where
    toJSON GameState{..} = object [
        "currLevel" .= _currLevel,
        "actorQueue" .= _actorQueue,
        "nextEntityRef" .= _nextEntityRef
        ]

instance FromJSON GameState where
    parseJSON = withObject "gamestate" $ \o -> do
        _currLevel <- o .: "currLevel"
        _actorQueue <- o .: "actorQueue"
        _nextEntityRef <- o .: "nextEntityRef"
        _entitiesByCoord <- return Nothing
        _obstructionByCoord <- return Nothing
        _visibleToPlayer <- return Nothing
        return GameState{..}
