module Main where

import Prelude hiding (Either(..), id, (.))
import Control.Applicative
import Control.Category
import Game
import Coord
import Entity
import GameState
import UI
import Level

gameState = mkLevel

main :: IO ()
main = do
    display <- initDisplay
    gameLoop display gameState
    endDisplay display
