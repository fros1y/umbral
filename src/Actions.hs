{-# LANGUAGE DeriveGeneric              #-}
module Actions where

import           Debug.Trace
import           Debug.Trace.Helpers
import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category
import           GHC.Generics
import           Control.Lens

import Coord
import Entity

data Action = ActPlayerTurnDone |
              ActWait |
              ActMoveBy DeltaCoord |
              ActAttack TargetEntityRef
              deriving (Show, Generic, Eq)

type ActionsByEntity = (EntityRef, [Action])

determineActionCost :: Action -> Int
determineActionCost ActPlayerTurnDone   = 0
determineActionCost _                   = 100

returnActionsFor :: Entity -> [Action] -> ActionsByEntity
returnActionsFor entity actions = (entity ^. entityRef, actions)
