{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Symbol where

import Control.Lens
import qualified Color as Color
import Data.Default
import GHC.Generics
import Prelude hiding (Either(..), id, (.))

-- Symbol
data Symbol = Symbol
    { _glyph :: Char
    , _baseColor :: Color.Color
    } deriving (Generic,Show)

makeLenses ''Symbol

instance Default Symbol where
    def = Symbol '?' $ Color.byName "white"
