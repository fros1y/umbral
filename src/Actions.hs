{-# LANGUAGE DeriveGeneric #-}

module Actions where

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Monad.Reader as Reader
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Debug.Trace
import Debug.Trace.Helpers
import GHC.Generics
import Prelude hiding (Either(..), id, (.))
import Coord
import Effects
import Entity
import Entity
import GameEngine
import GameState

data Action
    = ActWait
    | ActMoveBy DeltaCoord
    | ActAttack TargetEntityRef
    deriving (Show,Generic,Eq)

type ActionsByEntity = (EntityRef, [Action])

determineActionCost :: Action -> Int
determineActionCost _ = 100

returnActionsFor :: Entity -> [Action] -> ActionsByEntity
returnActionsFor entity actions = (entity ^. entityRef, actions)

-----
validateActions
    :: ActionsByEntity -> GameEngine [Action]
validateActions (ref,actions) = do
    e <- getEntity ref
    case e of
        Nothing -> return [] -- a non-existent entity can do nothing
        (Just e') -> filterM (validActionBy e') actions

applyActions :: ActionsByEntity -> GameEngine EffectsToEntities
applyActions (_,[]) = return mempty
applyActions actionsByEntity@(ref,actions) = do
    validActions <- validateActions actionsByEntity
    effects <- mapM (applyAction ref) validActions
    let cost =
            returnEffectsForRef
                ref
                [EffSpendAP $ (sum <<< (fmap determineActionCost)) validActions]
    return $ mconcat (cost : effects)

-----
validActionBy
    :: Entity -> Action -> GameEngine Bool
validActionBy e (ActMoveBy delta) = traversableAt $ (e ^. position) + delta
validActionBy e (ActAttack ref) = isAttackableRef ref
validActionBy _ _ = return True

applyAction :: EntityRef -> Action -> GameEngine EffectsToEntities
applyAction ref ActWait = return $ returnEffectsForRef ref [EffPass]
applyAction ref (ActAttack aref) =
    return $ returnEffectsForRef aref [EffDamaged 1]
applyAction ref (ActMoveBy delta) = do
    e <- fromJust <$> getEntity ref
    return (returnEffectsForRef ref [EffMoveTo $ (e ^. position) + delta])
applyAction ref _ = return mempty
