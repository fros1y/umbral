module Utils where

import           Debug.Trace
import           Debug.Trace.Helpers
import qualified Control.Monad.State  as State
import Control.Applicative

traceMsgM :: (Monad m, Show r) => [Char] -> m r -> m r
traceMsgM a = State.liftM $ traceMsg a

(+++) :: Applicative f => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)
