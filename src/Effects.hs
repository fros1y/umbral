
{-# LANGUAGE DeriveGeneric              #-}
module Effects where

import           Debug.Trace
import           Debug.Trace.Helpers
import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category
import           GHC.Generics
import           Control.Lens

import qualified Data.IntMap.Strict   as IntMap

import Coord
import Entity

data Effect = EffMoveTo Coord
            | EffDamaged Int
            | EffRecoverAP
            | EffSpendAP Int
            | EffDestroy
            deriving (Show, Generic, Eq)

--type EffectsToEntities = IntMap.IntMap [Effect]
newtype EffectsToEntities = EffectsToEntities {
  getMap :: IntMap.IntMap [Effect]
} deriving (Show)

instance Monoid EffectsToEntities where
  mempty = EffectsToEntities IntMap.empty
  mappend x y = EffectsToEntities $ IntMap.unionWith (++) (getMap x) (getMap y)
    -- The Monoid instance for IntMap does not append like I want

returnEffectsFor :: Entity -> [Effect] -> EffectsToEntities
returnEffectsFor entity effects = EffectsToEntities $ IntMap.singleton (entity ^. entityRef) effects

returnEffectsForRef :: EntityRef -> [Effect] -> EffectsToEntities
returnEffectsForRef entityref effects = EffectsToEntities $ IntMap.singleton entityref effects

returnEffectsForAll :: [Effect] -> EffectsToEntities
returnEffectsForAll effects = EffectsToEntities $ IntMap.singleton (-1) effects

applyEffects :: EntityRef -> [Effect] -> Entity -> Maybe Entity
applyEffects _ effects e = foldr applyEffect (Just e) effects

applyBroadcastEffects :: Maybe [Effect] -> IntMap.IntMap Entity -> IntMap.IntMap Entity
applyBroadcastEffects Nothing ents = ents
applyBroadcastEffects (Just effs) ents = IntMap.mapMaybeWithKey apply' ents where
  apply' :: EntityRef -> Entity -> Maybe Entity
  apply' ref ent = applyEffects ref effs ent

applyEffect :: Effect -> Maybe Entity -> Maybe Entity
applyEffect EffDestroy          e = traceMsg "EffDestroy: " Nothing
applyEffect EffRecoverAP        e = traceMsg "EffRecoverAP: " $ pure recoverAP <*> e
applyEffect (EffSpendAP ap)     e = traceMsg "EffSpendAP: " $ spendAP <$> e <*> pure ap
applyEffect (EffDamaged dmg)    e = traceMsg "EffDamaged: " $ applyDamage <$> e <*> pure dmg
applyEffect (EffMoveTo pos)     e = traceMsg "EffMoveTo: " $ moveTo <$> e <*> pure pos
-- applyEffect _                   e = traceMsg "UNHANDLED EFF" $ e


spendAP :: Entity -> Int -> Entity
spendAP e apC = e & actor %~ (liftA spendAP') where
  spendAP' act = act & actionPoints -~ apC

recoverAP :: Entity -> Entity
recoverAP e = e & actor %~ (liftA recoverAP') where
  recoverAP' act = act & actionPoints +~ (act ^. speed)

moveTo :: Entity -> Coord -> Entity
moveTo e coord = e & position .~ coord

applyDamage :: Entity -> Int -> Entity
applyDamage e dmg = e & health %~ (liftA applyDamage') where
  applyDamage' hp = hp & currHP -~ dmg
