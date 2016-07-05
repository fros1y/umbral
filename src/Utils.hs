module Utils where

import Control.Applicative
import qualified Control.Monad.State as State
import Debug.Trace.Helpers

traceMsgM
    :: (Monad m, Show r)
    => [Char] -> m r -> m r
traceMsgM a = State.liftM $ traceMsg a

(+++)
    :: Applicative f
    => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)

clamp :: (Ord a) => a -> a -> a -> a
clamp minV maxV inV = (min (max inV minV) maxV)
