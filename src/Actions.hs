{-# LANGUAGE DeriveGeneric              #-}
module Actions where

import           Debug.Trace
import           Debug.Trace.Helpers
import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category
import           GHC.Generics
import           Control.Lens
import           Data.Maybe           (fromJust, isJust, isNothing, listToMaybe)
import           Control.Monad.Reader as Reader
import Coord
import Entity
import Effects
import Entity
import GameM
import GameState

data Action =
              ActWait |
              ActMoveBy DeltaCoord |
              ActAttack TargetEntityRef
              deriving (Show, Generic, Eq)

type ActionsByEntity = (EntityRef, [Action])

determineActionCost :: Action -> Int
determineActionCost _                   = 100

returnActionsFor :: Entity -> [Action] -> ActionsByEntity
returnActionsFor entity actions = (entity ^. entityRef, actions)

-----

validateActions :: ActionsByEntity -> GameM [Action]
validateActions (ref, actions) = do
  e <- getEntity ref
  case e of
    Nothing -> return [] -- a non-existent entity can do nothing
    (Just e') -> filterM (validActionBy e') actions

applyActions :: ActionsByEntity -> GameM EffectsToEntities
applyActions (_, []) = return mempty
applyActions actionsByEntity@(ref, actions) = do
  validActions <- validateActions actionsByEntity
  effects <- mapM (applyAction ref) validActions
  let cost = returnEffectsForRef ref [EffSpendAP $ (sum <<< (fmap determineActionCost)) validActions]
  return $ mconcat (cost:effects)
-----

validActionBy :: Entity -> Action -> GameM Bool
validActionBy e (ActMoveBy delta) = traversableAt $ (e ^. position) + delta
validActionBy e (ActAttack ref) = isAttackableRef ref
validActionBy _ _ = return True

applyAction :: EntityRef -> Action -> GameM EffectsToEntities
applyAction ref ActWait             = return $ returnEffectsForRef ref [EffPass]
applyAction ref (ActAttack aref)    = return $ returnEffectsForRef aref [EffDamaged 1]
applyAction ref (ActMoveBy delta)   = do
  e <- fromJust <$> getEntity ref
  return (returnEffectsForRef ref [EffMoveTo $ (e ^. position) + delta])
applyAction ref _                   = return mempty
