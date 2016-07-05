{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Lighting where

import Control.Lens
import qualified Color as Color
import Data.Monoid
import Data.Default
import GHC.Generics
import Prelude hiding (Either(..), id, (.))

data LightSource = LightSource
    { _brightness :: Double
    , _lightColor :: Color.Color
    } deriving (Generic,Show)

makeLenses ''LightSource

instance Default LightSource where
    def = LightSource 1 (Color.byName "white")

data LightLevel = LightLevel {
    _litBrightness :: Double,
    _litColor :: Color.Color
} deriving (Generic, Show)

makeLenses ''LightLevel

instance Monoid LightLevel where
    mempty = LightLevel 0.0 (Color.byName "black")
    mappend l1 l2 = LightLevel  ((l1 ^. litBrightness) + (l2 ^. litBrightness))
                                ((l1 ^. litColor) <> (l2 ^. litColor))


instance Default LightLevel where
    def = mempty

castLight :: LightSource -> Double -> LightLevel
castLight lightSource dist = LightLevel intensity color where
    color = lightSource ^. lightColor
    intensity = (1 / (max dist 1)^2) * (lightSource ^. brightness)

apparentColor :: Color.Color -> LightLevel -> Color.Color
apparentColor color lightLevel = color * lightLevel ^. litColor
