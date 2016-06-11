{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Data.Default
import           Data.Maybe          (fromJust, isJust, isNothing, listToMaybe)
import           Debug.Trace
import           Debug.Trace.Helpers
import           GHC.Generics
import           Prelude             hiding (Either (..), id, (.))

import           Coord
import           Symbol


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
  _entityRef   :: EntityRef,
  _position    :: Coord,
  _symbol      :: Symbol,
  _health      :: Maybe Health,
  _actor       :: Maybe Actor,
  _obstruction :: Maybe Obstruction
} deriving (Show, Generic)
makeLenses ''Entity

entityStrategy :: Entity -> Maybe Strategy
entityStrategy e = getStrategy <$> getActor e where
  getActor e = e ^. actor
  getStrategy a = a ^. strategy

isPlayerEntity :: Entity -> Bool
isPlayerEntity e = case e ^. actor of
  Nothing -> False
  Just a -> (a ^. strategy) == Player

entitiesAt :: Coord -> [Entity] -> [Entity]
entitiesAt coord entities = filter (entityAt coord) entities where
  entityAt :: Coord -> Entity -> Bool
  entityAt coord e = (coord == e ^. position)

isTraversable :: Entity -> Bool
isTraversable e = maybe True checkTraversable (e ^. obstruction) where
  checkTraversable ob = ob ^. traversable

isAttackable :: Entity -> Bool
isAttackable e = isJust (e ^. health)

isDead :: Entity -> Bool
isDead e = case e ^. health of
  Nothing -> False
  (Just hp) -> hp ^. currHP <= 0

mkBaseEntity :: Coord -> Symbol -> Entity
mkBaseEntity coord sym = Entity {       _entityRef = -99,
                                        _position = coord,
                                        _symbol = sym,
                                        _health = Nothing,
                                        _actor = Nothing,
                                        _obstruction = def
                                    }

mkPlayer :: Coord -> Entity
mkPlayer coord = baseEntity & health .~ Just (mkHealth 100)
                            & actor .~ Just (mkActor Player)
                    where
                      baseEntity = mkBaseEntity coord sym
                      sym = def & glyph .~ '@'

mkRandomRat :: Coord -> Entity
mkRandomRat coord = baseEntity  & health .~ Just (mkHealth 1)
                                & actor .~ Just (mkActor Random)
                    where
                      baseEntity = mkBaseEntity coord sym
                      sym = def & glyph .~ 'r'

mkZombie :: Coord -> Entity
mkZombie coord = baseEntity & health .~ Just (mkHealth 1)
                            & actor .~ Just (( mkActor Zombie) & speed .~ 50)
                    where
                      baseEntity = mkBaseEntity coord sym
                      sym = def & glyph .~ 'Z'

mkWall :: Coord -> Entity
mkWall coord = baseEntity where
  sym = def & glyph .~ '#'
  baseEntity = mkBaseEntity coord sym
