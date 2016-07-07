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
    mappend l1 l2 = LightLevel  (brightness1 + brightness2)
                                (scaledColor1 + scaledColor2)
                    where
                        brightness1 = (l1 ^. litBrightness)
                        brightness2 = (l2 ^. litBrightness)
                        factor1 = brightness1 / (brightness1 + brightness2)
                        factor2 = (1 - factor1)
                        scaledColor1 = Color.scale factor1 (l1 ^. litColor)
                        scaledColor2 = Color.scale factor2 (l2 ^. litColor)



instance Default LightLevel where
    def = mempty

castLight :: LightSource -> Double -> LightLevel
castLight lightSource dist = LightLevel intensity color where
    color = lightSource ^. lightColor
    intensity = (1 / (max dist 1)^2) * (lightSource ^. brightness)

apparentColor :: LightLevel -> Color.Color -> Color.Color
apparentColor lightLevel color = Color.scale (min 1.0 (lightLevel ^. litBrightness)) (color * lightLevel ^. litColor)

aboveThreshold :: Double -> LightLevel -> Bool
aboveThreshold threshold level = (level ^. litBrightness) > threshold
