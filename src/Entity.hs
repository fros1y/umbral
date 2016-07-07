{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Maybe (isJust)
import GHC.Generics
import Prelude hiding (Either(..), id, (.))
import qualified Color as Color

import Coord
import Symbol
import Lighting


type EntityRef = Int

type TargetEntityRef = EntityRef

data Health = Health
    { _currHP :: Int
    , _maxHP :: Int
    } deriving (Show,Generic)

makeLenses ''Health

mkHealth :: Int -> Health
mkHealth maxHealth =
    Health
    { _currHP = maxHealth
    , _maxHP = maxHealth
    }

data Strategy
    = Player
    | Zombie
    | Random
    deriving (Show,Generic,Eq)

data Actor = Actor
    { _strategy :: Strategy
    , _actionPoints :: Int
    , _speed :: Int
    } deriving (Show,Generic)

makeLenses ''Actor

data Obstruction = Obstruction
    { _traversable :: Bool
    , _transparent :: Bool
    } deriving (Show,Generic)

makeLenses ''Obstruction

instance Monoid Obstruction where
  mappend o0 o1 = Obstruction (o0 ^. traversable && o1 ^. traversable)
                              (o0 ^. transparent && o1 ^. transparent)
  mempty = Obstruction True True

instance Default Obstruction where
    def =
        Obstruction
        { _traversable = False
        , _transparent = False
        }

mkActor :: Strategy -> Actor
mkActor strat =
    Actor
    { _strategy = strat
    , _actionPoints = 100
    , _speed = 100
    }

data Entity = Entity
    { _entityRef :: EntityRef
    , _position :: Coord
    , _symbol :: Symbol
    , _health :: Maybe Health
    , _actor :: Maybe Actor
    , _obstruction :: Maybe Obstruction
    , _lightSource :: Maybe LightSource
    } deriving (Show,Generic)

makeLenses ''Entity

entityStrategy :: Entity -> Maybe Strategy
entityStrategy e = getStrategy <$> (e ^. actor)
  where
    getStrategy a = a ^. strategy

isPlayerEntity :: Entity -> Bool
isPlayerEntity e =
    case e ^. actor of
        Nothing -> False
        Just a -> (a ^. strategy) == Player

isTraversable :: Entity -> Bool
isTraversable e = maybe True checkTraversable (e ^. obstruction)
  where
    checkTraversable ob = ob ^. traversable

isOpaque :: Entity -> Bool
isOpaque e = maybe False checkOpaque (e^.obstruction)
  where
    checkOpaque ob = not (ob ^. transparent)

isAttackable :: Entity -> Bool
isAttackable e = isJust (e ^. health)

isDead :: Entity -> Bool
isDead e =
    case e ^. health of
        Nothing -> False
        (Just hp) -> hp ^. currHP <= 0


isLightSource :: Entity -> Bool
isLightSource e = isJust (e ^. lightSource)

mkBaseEntity :: Coord -> Symbol -> Entity
mkBaseEntity coord sym =
    Entity
    { _entityRef = -99
    , _position = coord
    , _symbol = sym
    , _health = Nothing
    , _actor = Nothing
    , _obstruction = Just def
    , _lightSource = Nothing
    }

mkPlayer :: Coord -> Entity
mkPlayer coord =
    baseEntity  & health .~ Just (mkHealth 100)
                & actor .~ Just (mkActor Player)
                & lightSource .~ Just (LightSource 5 (Color.byName "light yellow"))
  where
    baseEntity = mkBaseEntity coord sym
    sym = def & glyph .~ '@'

mkRandomRat :: Coord -> Entity
mkRandomRat coord =
    baseEntity & health .~ Just (mkHealth 1) & actor .~ Just (mkActor Random)
  where
    baseEntity = mkBaseEntity coord sym
    sym = def & glyph .~ 'r'

mkZombie :: Coord -> Entity
mkZombie coord =
    baseEntity & health .~ Just (mkHealth 1) & actor .~
    Just ((mkActor Zombie) & speed .~ 50)
  where
    baseEntity = mkBaseEntity coord sym
    sym = def & glyph .~ 'Z'

mkWall :: Coord -> Entity
mkWall coord = baseEntity
  where
    sym = def & glyph .~ '#'
    baseEntity = mkBaseEntity coord sym

mkFloor :: Coord -> Entity
mkFloor coord = baseEntity & obstruction .~ Nothing
  where
    sym = def & glyph .~ '⋅'
    baseEntity = mkBaseEntity coord sym

addLight :: LightSource -> Entity -> Entity
addLight light e = e & lightSource .~ Just light

mkLitColumn :: Coord -> Entity
mkLitColumn coord = addLight (LightSource 1 $ Color.byName "steel blue") (mkWall coord)
