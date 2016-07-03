{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serialize where

import Data.Aeson
import GHC.Generics
import Data.Colour as Colour
import Data.Colour.Names as Colour
import Data.Dequeue
import Coord
import Entity
import Symbol
import GameState

instance ToJSON Coord

instance FromJSON Coord

instance ToJSON Bounds

instance FromJSON Bounds

instance ToJSON (Colour.Colour Double) where
    toJSON = toJSON . show

instance FromJSON (Colour.Colour Double) where
    parseJSON = fmap read . parseJSON

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

instance ToJSON GameState where
    toJSON GameState{..} = object [
        "gameEntities" .= _gameEntities,
        "actorQueue" .= _actorQueue,
        "nextEntityRef" .= _nextEntityRef,
        "bounding" .= _bounding
        ]

instance FromJSON GameState where
    parseJSON = withObject "gamestate" $ \o -> do
        _gameEntities <- o .: "gameEntities"
        _actorQueue <- o .: "actorQueue"
        _nextEntityRef <- o .: "nextEntityRef"
        _bounding <- o .: "bounding"
        _entitiesByCoord <- return Nothing
        _obstructionByCoord <- return Nothing
        _visibleToPlayer <- return Nothing
        return GameState{..}
