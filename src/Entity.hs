{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Entity where

import           Control.Lens
import           GHC.Generics

import           Debug.Trace
import           Debug.Trace.Helpers
import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category

import Symbol
import Coord
import           Data.Default

type EntityRef       = Int
type TargetEntityRef = EntityRef

data Health = Health {
  _currHP :: Int,
  _maxHP  :: Int
} deriving (Show, Generic)
makeLenses ''Health

mkHealth :: Int -> Health
mkHealth maxHealth = Health {_currHP = maxHealth, _maxHP = maxHealth}

data Strategy = Player
              | Zombie
              | Random
              deriving (Show, Generic, Eq)

data Actor = Actor {
  _strategy     :: Strategy,
  _actionPoints :: Int,
  _speed        :: Int
} deriving (Show, Generic)
makeLenses ''Actor

data Obstruction = Obstruction {
  _traversable :: Bool,
  _transparent :: Bool
} deriving (Show, Generic)
makeLenses ''Obstruction

instance Default Obstruction where
  def = Obstruction {_traversable = False, _transparent = False}

mkActor :: Strategy -> Actor
mkActor strat = Actor {_strategy = strat, _actionPoints = 100, _speed = 100}

data Entity = Entity {
  _entityRef :: EntityRef,
  _position  :: Coord,
  _symbol    :: Symbol,
  _health    :: Maybe Health,
  _actor     :: Maybe Actor,
  _obstruction :: Maybe Obstruction
} deriving (Show, Generic)
makeLenses ''Entity


isPlayerEntity :: Entity -> Bool
isPlayerEntity e = case e ^. actor of
  Nothing -> False
  Just a -> (a ^. strategy) == Player

mkBaseEntity :: EntityRef -> Coord -> Symbol -> Entity
mkBaseEntity ref coord sym = Entity {   _entityRef = ref,
                                        _position = coord,
                                        _symbol = sym,
                                        _health = Nothing,
                                        _actor = Nothing,
                                        _obstruction = def
                                    }

mkPlayer :: EntityRef -> Coord -> Entity
mkPlayer ref coord = baseEntity & health .~ Just (mkHealth 100)
                                & actor .~ Just (mkActor Player)
                    where
                      baseEntity = mkBaseEntity ref coord sym
                      sym = def & glyph .~ '@'

mkRandomRat :: EntityRef -> Coord -> Entity
mkRandomRat ref coord = baseEntity  & health .~ Just (mkHealth 1)
                                    & actor .~ Just (mkActor Random)
                    where
                      baseEntity = mkBaseEntity ref coord sym
                      sym = def & glyph .~ 'r'

mkWall :: EntityRef -> Coord -> Entity
mkWall ref coord = baseEntity where
  sym = def & glyph .~ '#'
  baseEntity = mkBaseEntity ref coord sym
