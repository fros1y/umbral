{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Game where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad        (when)
import qualified Control.Monad.Random as Random
import           Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State
import           Coord
import           Data.Default
import qualified Data.Dequeue         as DQ
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isJust, isNothing)
import           GHC.Generics
import           Prelude              hiding (Either (..), id, (.))
import           Symbol
import Debug.Trace
import Debug.Trace.Helpers
import qualified Control.Monad.Loops as L
import Control.Concurrent

type EntityRef       = Int
type TargetEntityRef = EntityRef

data Health = Health {
  _currHP :: Int,
  _maxHP  :: Int
} deriving (Show, Generic)
makeLenses ''Health

mkHealth :: Int -> Health
mkHealth maxHealth = Health {_currHP = maxHealth, _maxHP = maxHealth}

data Strategy = Player
              | Inanimate
              | Zombie
              | Random
              deriving (Show, Generic, Eq)

data Actor = Actor {
  _strategy     :: Strategy,
  _actionPoints :: Int,
  _speed        :: Int
} deriving (Show, Generic)
makeLenses ''Actor

mkActor :: Strategy -> Actor
mkActor strat = Actor {_strategy = strat, _actionPoints = 100, _speed = 100}

data Entity = Entity {
  _entityRef :: EntityRef,
  _position  :: Coord,
  _symbol    :: Symbol,
  _health    :: Maybe Health,
  _actor     :: Maybe Actor
} deriving (Show, Generic)
makeLenses ''Entity

mkBaseEntity :: EntityRef -> Coord -> Symbol -> Entity
mkBaseEntity ref coord sym = Entity {   _entityRef = ref,
                                        _position = coord,
                                        _symbol = sym,
                                        _health = Nothing,
                                        _actor = Nothing
                                    }

mkPlayer :: EntityRef -> Coord -> Entity
mkPlayer ref coord = baseEntity & health .~ Just (mkHealth 100)
                                & actor .~ Just (mkActor Player)
                    where
                      baseEntity = mkBaseEntity ref coord sym
                      sym = def & glyph .~ '@'

mkRandomRat :: EntityRef -> Coord -> Entity
mkRandomRat ref coord = baseEntity  & health .~ Just (mkHealth 1)
                                    & actor .~ Just (mkActor Random)
                        where
                          baseEntity = mkBaseEntity ref coord sym
                          sym = def & glyph .~ 'r'

mkWall :: EntityRef -> Coord -> Entity
mkWall ref coord = baseEntity where
  sym = def & glyph .~ '#'
  baseEntity = mkBaseEntity ref coord sym

type ActorQueue = DQ.BankersDequeue EntityRef

data GameState = GameState {
  _gameEntities :: IntMap.IntMap Entity,
  _actorQueue   :: ActorQueue
} deriving (Show, Generic)
makeLenses ''GameState

mkGameState :: Coord -> GameState
mkGameState playerStart = GameState {
                            _gameEntities = entities,
                            _actorQueue = queue
                          }
                          where
                            player = mkPlayer 1 playerStart
                            entities = IntMap.singleton 1 player
                            queue = DQ.fromList [1]

addEntityToGame :: Entity -> GameState -> GameState
addEntityToGame entity gameState = gameState  & gameEntities %~ addEntity
                                              & actorQueue %~ addQueue
                            where
                              ref = entity ^. entityRef
                              addEntity ents = IntMap.insert ref entity ents
                              addQueue queue =  if isJust $ entity ^. actor
                                                then DQ.pushBack queue ref
                                                else queue

addEntitiesToGame :: [Entity] -> GameState -> GameState
addEntitiesToGame ents gameState = Prelude.foldr addEntityToGame gameState ents

instance Default GameState where
  def = mkGameState (Coord 0 0)

data Action = ActPlayerTurnDone |
              ActWait |
              ActMoveBy DeltaCoord |
              ActAttack TargetEntityRef
              deriving (Show, Generic, Eq)

type ActionsByEntity = (EntityRef, [Action])

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

newtype GameM a = GameM {
  runGame :: (Reader.ReaderT GameState IO) a
} deriving (Functor, Applicative, Random.MonadRandom, Monad, MonadReader GameState, State.MonadIO)

traceMsgM :: (Monad m, Show r) => [Char] -> m r -> m r
traceMsgM a = State.liftM $ traceMsg a

iterateM :: (Monad m) => (a -> m a) -> m a -> Int -> m ()
iterateM f mx n = sequence_ . take n . iterate (>>= f) $ mx

gameLoop :: GameState -> IO ()
gameLoop gameState = -- iterateM gameStep (return gameState) 10
                      L.iterateM_ gameStep gameState

gameStep :: GameState -> IO GameState
gameStep gameState = do
  threadDelay 10000
  Reader.runReaderT (runGame gameStepM) gameState

gameStepM :: GameM GameState
gameStepM = do
  gameState <- traceMsgM "gameStep start: " ask
  render
  potentialEntityToRun <- traceMsgM "entityToRun: " getEntityToRun
  case potentialEntityToRun of
    Nothing -> do
      return $ traceMsg "gameStep finish (Nothing): " (gameState & actorQueue %~ rotate)
    Just entityToRun -> do
      let entityToRun = fromJust potentialEntityToRun
      actions <- if isPlayerEntity entityToRun
                then traceMsgM "getPlayerActions: " $ getPlayerActions entityToRun
                else traceMsgM "runEntity: " $ runEntity entityToRun
      effects <-    traceMsgM "applyActions: " $ applyActions actions
      applyEffectsToEntities effects -- return mutated gameState

render :: GameM ()
render = do
  state <- ask
  liftIO $ print state

isPlayerEntity :: Entity -> Bool
isPlayerEntity e = case e ^. actor of
  Nothing -> False
  Just a -> (a ^. strategy) == Player

getEntity :: EntityRef -> GameM (Maybe Entity)
getEntity ref = do
  state <- ask
  return $ IntMap.lookup ref (state ^. gameEntities)

rotate :: ActorQueue -> ActorQueue
rotate queue = queueChoice where
  potentialQ = do
    (exiting, queue') <- DQ.popFront queue
    return $ DQ.pushBack queue' exiting
  queueChoice = case potentialQ of Nothing -> queue
                                   (Just q) -> q

actionPointsOfEntity :: Maybe Entity -> Maybe Int
actionPointsOfEntity eM = do
  e <- eM
  actor' <- e ^. actor
  return $ actor' ^. actionPoints

enoughActionPoints :: Maybe Int -> Bool
enoughActionPoints p = case p of
  Nothing -> False
  Just p' -> p' > 0

stillActive :: Maybe Entity -> Bool
stillActive = (enoughActionPoints <<< actionPointsOfEntity)

firstInQueue :: GameM (Maybe Entity)
firstInQueue = do
  state <- ask
  let ref = (DQ.first (state ^. actorQueue)) :: Maybe EntityRef
  case ref of Nothing -> return Nothing
              Just r -> getEntity r

getEntityToRun :: GameM (Maybe Entity)
getEntityToRun = do
  candidate <- firstInQueue
  if stillActive candidate
    then return candidate
    else return Nothing

returnActionsFor :: Entity -> [Action] -> ActionsByEntity
returnActionsFor entity actions = (entity ^. entityRef, actions)

returnEffectsFor :: Entity -> [Effect] -> EffectsToEntities
returnEffectsFor entity effects = EffectsToEntities $ IntMap.singleton (entity ^. entityRef) effects

returnEffectsForRef :: EntityRef -> [Effect] -> EffectsToEntities
returnEffectsForRef entityref effects = EffectsToEntities $ IntMap.singleton entityref effects

returnEffectsForAll :: [Effect] -> EffectsToEntities
returnEffectsForAll effects = EffectsToEntities $ IntMap.singleton (-1) effects

applyActions :: ActionsByEntity -> GameM EffectsToEntities
applyActions actionsByEntity@(ref, actions) = do
  validActions <- validateActions actionsByEntity
  effects <- mapM (applyAction ref) validActions
  let cost = returnEffectsForRef ref [EffSpendAP $ (sum <<< (fmap determineActionCost)) validActions]
  return $ mconcat (cost:effects)

------

directionFromInput :: Char -> Maybe Direction
directionFromInput char = case char of
    'h' -> Just Coord.Left
    'l' -> Just Coord.Right
    'j' -> Just Coord.Down
    'k' -> Just Coord.Up
    _ -> Nothing

getPlayerActions :: Entity -> GameM ActionsByEntity
getPlayerActions player = do
  input <- Reader.liftIO getChar
  let dir = directionFromInput input
      act = case dir of Nothing -> []
                        (Just d) -> [ActMoveBy $ fromDirection d,
                                     ActPlayerTurnDone]
  return $ returnActionsFor player act

-----

getRandomDirection :: (Random.MonadRandom m) => m Direction
getRandomDirection = Random.uniform [Coord.Left, Coord.Right, Coord.Down, Coord.Up]

------

runEntity :: Entity -> GameM ActionsByEntity
runEntity entity = do
  randomDirection <- getRandomDirection
  return $ returnActionsFor entity [ActMoveBy $ fromDirection randomDirection]

-------

validateActions :: ActionsByEntity -> GameM [Action]
validateActions (ref, actions) = return actions

-------

applyAction :: EntityRef -> Action -> GameM EffectsToEntities
applyAction ref ActPlayerTurnDone   = return $ returnEffectsForAll [EffRecoverAP]
applyAction ref (ActMoveBy delta)   = do
  e <- fromJust <$> getEntity ref
  return (returnEffectsForRef ref [EffMoveTo $ (e ^. position) + delta])
applyAction ref _                   = return mempty

-------

applyEffects :: EntityRef -> [Effect] -> Entity -> Maybe Entity
applyEffects _ effects e = foldr applyEffect (Just e) effects

applyEffectsToEntities :: EffectsToEntities -> GameM GameState
applyEffectsToEntities effects = do
  gameState <- ask
  let gameEntities' = IntMap.mergeWithKey applyEffects (const IntMap.empty) id (getMap effects) (gameState ^. gameEntities)
      gameEntities'' = applyBroadcastEffects (IntMap.lookup (-1) (getMap effects)) gameEntities'
      gameState'    = gameState
                    & gameEntities .~ gameEntities''
  return $ gameState'

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

determineActionCost :: Action -> Int
determineActionCost ActPlayerTurnDone   = 0
determineActionCost _                   = 100


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
