{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Serialize where

import           Data.Aeson
import           GHC.Generics
import           Data.Colour         as Colour
import           Data.Colour.Names   as Colour
import            Data.Dequeue

import Coord
import Entity
import Symbol
import GameState

instance ToJSON Coord
instance FromJSON Coord

instance ToJSON (Colour.Colour Double) where
  toJSON = toJSON . show

instance FromJSON (Colour.Colour Double) where
  parseJSON  = fmap read . parseJSON

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

instance ToJSON GameState
instance FromJSON GameState
