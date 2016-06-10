
{-# LANGUAGE DeriveGeneric #-}
module Effects where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad.Reader as Reader
import qualified Data.IntMap.Strict   as IntMap
import           Debug.Trace
import           Debug.Trace.Helpers
import           GHC.Generics
import           Prelude              hiding (Either (..), id, (.))

import           Coord
import           Entity
import           GameM
import           GameState

data Effect = EffMoveTo Coord
            | EffDamaged Int
            | EffRecoverAP
            | EffSpendAP Int
            | EffDestroy
            | EffPass
            deriving (Show, Generic, Eq)

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

-----

applyEffectsToEntities :: EffectsToEntities -> GameM GameState
applyEffectsToEntities effects = do
  gameState <- ask
  let gameEntities' = IntMap.mergeWithKey applyEffects (const IntMap.empty) id (getMap effects) (gameState ^. gameEntities)
      gameState'    = gameState
                    & gameEntities .~ gameEntities'
  return $ gameState'

applyEffects :: EntityRef -> [Effect] -> Entity -> Maybe Entity
applyEffects _ effects e = foldr applyEffect (Just e) effects

applyEffect :: Effect -> Maybe Entity -> Maybe Entity
applyEffect EffDestroy          e = Nothing
applyEffect EffRecoverAP        e = pure recoverAP <*> e
applyEffect (EffSpendAP ap)     e = spendAP <$> e <*> pure ap
applyEffect (EffDamaged dmg)    e = do
  e' <- applyDamage <$> e <*> pure dmg
  if isDead e'
    then Nothing
    else Just e'
applyEffect (EffMoveTo pos)     e = moveTo <$> e <*> pure pos
applyEffect EffPass             e = pure spendAllAP <*> e
-- applyEffect _                   e = traceMsg "UNHANDLED EFF" $ e

--------

spendAllAP :: Entity -> Entity
spendAllAP e = e & actor %~ (liftA spendAllAP') where
  spendAllAP' act = act & actionPoints .~ 0

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
