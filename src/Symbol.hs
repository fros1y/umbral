{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Symbol where

import Control.Lens
import Data.Colour as Colour
import Data.Colour.Names as Colour
import Data.Default
import GHC.Generics
import Prelude hiding (Either(..), id, (.))

-- Symbol
data Symbol = Symbol
    { _glyph :: Char
    , _baseColor :: Colour.Colour Double
    } deriving (Generic,Show)

makeLenses ''Symbol

instance Default Symbol where
    def = Symbol '?' Colour.white
