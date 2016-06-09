module Utils where

import           Debug.Trace
import           Debug.Trace.Helpers
import qualified Control.Monad.State  as State


traceMsgM :: (Monad m, Show r) => [Char] -> m r -> m r
traceMsgM a = State.liftM $ traceMsg a
