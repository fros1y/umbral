module ActorQueue where

import Control.Category
import Control.Lens
import qualified Data.Dequeue as DQ
import Prelude hiding (Either(..), id, (.))
import Entity

type ActorQueue = DQ.BankersDequeue EntityRef

dropFront :: ActorQueue -> ActorQueue
dropFront queue =
    case DQ.popFront queue of
        Nothing -> DQ.empty
        Just (_ ,q) -> q

rotate :: ActorQueue -> ActorQueue
rotate queue = queueChoice
  where
    potentialQ = do
        (exiting,queue') <- DQ.popFront queue
        return $ DQ.pushBack queue' exiting
    queueChoice =
        case potentialQ of
            Nothing -> queue
            (Just q) -> q

actionPointsOfEntity :: Maybe Entity -> Maybe Int
actionPointsOfEntity eM = do
    e <- eM
    actor' <- e ^. actor
    return $ actor' ^. actionPoints

enoughActionPoints :: Maybe Int -> Bool
enoughActionPoints p =
    case p of
        Nothing -> False
        Just p' -> p' > 0

stillActive :: Maybe Entity -> Bool
stillActive = (enoughActionPoints <<< actionPointsOfEntity)
