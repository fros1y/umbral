{-# LANGUAGE GADTs #-}
module GameOperational where

import qualified System.Random.Shuffle as Random
import Control.Monad.Operational
import Control.Monad

import Coord

type EntityMap = Int
data EntityAction = Increment
                  | Decrement
type ActionResult = Bool

data EntityState = EntityState {count :: Int} deriving Show

data EntityEngineI a where
    PerceiveWorld :: EntityEngineI EntityMap
    PerceiveSelf :: EntityEngineI EntityState
    Act :: EntityAction -> EntityEngineI ActionResult
    RandomDecision :: [d] -> EntityEngineI d

type EntityEngine a = ProgramT EntityEngineI IO a

perceiveWorld :: EntityEngine EntityMap
perceiveWorld = singleton PerceiveWorld

perceiveSelf :: EntityEngine EntityState
perceiveSelf = singleton PerceiveSelf

act :: EntityAction -> EntityEngine ActionResult
act = singleton . Act

randomDecision :: [d] -> EntityEngine d
randomDecision = singleton . RandomDecision

runEntity ::  ProgramT EntityEngineI (ProgramT GameEngineI IO) a -> EntityState -> ProgramT GameEngineI IO (EntityState, a)
runEntity entityEngine = \s -> viewT entityEngine >>= \p -> eval p s
    where
    eval :: ProgramViewT EntityEngineI (ProgramT GameEngineI IO) a -> (EntityState -> ProgramT GameEngineI IO (EntityState, a))
    eval (Return x) = \s -> return (s, x)
    eval (PerceiveWorld :>>= k) = \s -> do
        entityMap <- entityPerception s
        runEntity (k entityMap) s
    eval (PerceiveSelf :>>= k) = \s -> runEntity (k s) s
    eval (Act a :>>= k) = \s -> do
        (result, s') <- entityAction s a
        runEntity (k result) s'
    eval (RandomDecision ds :>>= k) = \s -> do
        pick <- randomSelection ds
        runEntity (k pick) s

randomSelection :: [a] -> GameEngine a
randomSelection = singleton . RandomSelection

entityAction :: EntityState -> EntityAction -> GameEngine (ActionResult, EntityState)
entityAction s a = singleton $ EntityAction s a

entityPerception :: EntityState -> GameEngine EntityMap
entityPerception = singleton . EntityPerception

activeEntity :: GameEngine EntityState
activeEntity = singleton ActiveEntity

data GameState = GameState {turnCount :: Int, entities :: [EntityState]} deriving Show

data GameEngineI a where
    EntityAction :: EntityState -> EntityAction -> GameEngineI (ActionResult, EntityState)
    EntityPerception :: EntityState -> GameEngineI EntityMap
    RandomSelection :: [d] -> GameEngineI d
    ActiveEntity :: GameEngineI EntityState

type GameEngine a = ProgramT GameEngineI IO a

runGame :: GameEngine a -> GameState -> IO a
runGame gameEngine = \s -> viewT gameEngine >>= \p -> eval p s
    where
    eval :: ProgramViewT GameEngineI IO a -> (GameState -> IO a)
    eval (Return x) = \_ -> return x
    eval (EntityAction e a :>>= k) = \s -> do
        (result, e', s') <- applyAction a e s
        runGame (k (result, e')) s'
    eval (EntityPerception e :>>= k) = \s -> do
        entityMap <- getEntityMap e s
        runGame (k entityMap) s
    eval (RandomSelection ds :>>= k) = \s -> do
        selections <- Random.shuffleM ds
        runGame (k $ head selections) s
    eval (ActiveEntity :>>= k) = \s -> do
        active <- getActiveEntity s
        runGame (k active) s

applyAction :: EntityAction -> EntityState -> GameState -> IO (ActionResult, EntityState, GameState)
applyAction action entityState gameState = do
    (result, entityState', gameState') <- eval action
    return (gameState', entityState', result)
        where
            eval Increment = do
                putStrLn "inc"
                return (gameState, entityState, True)
            eval Decrement = do
                putStrLn "dec"
                return (gameState, entityState, True)

getEntityMap :: EntityState -> GameState -> IO EntityMap
getEntityMap entityState gameState = return 0

getActiveEntity = undefined

-- applyAction :: GameAction -> GameState -> (ActionResult, GameState)
-- applyAction AInc state = (True, state {a = a state + 1})
-- applyAction BInc state = (True, state {b = b state + 1})
--
--

--
-- getState :: GameEngine GameState
-- getState = singleton GetState
--
-- act :: GameAction -> GameEngine ActionResult
-- act = singleton . Act
--
-- msg :: String -> GameEngine ()
-- msg = singleton . Msg
--
-- runGame :: GameEngine a -> GameState -> IO a
-- runGame gameEngine = \s -> viewT gameEngine >>= \p -> eval p s
--     where
--     eval :: ProgramViewT GameEngineI IO a -> (GameState -> IO a)
--     eval (Return x) = \_ -> return x
--     eval (GetState :>>= k) = \s -> runGame (k s) s
--     eval (Act a :>>= k) = \s -> do
--         let (result, s') = applyAction a s
--         runGame (k result) s'
--     eval (Msg msg :>>= k) = \s -> do
--         putStrLn msg
--         runGame (k ()) s
--
--
-- test = runGame $ forever $ do
--     act AInc
--     act AInc
--     act BInc
--     msg "test foo"
--     s <- getState
--     msg $ "state: " ++ show s
