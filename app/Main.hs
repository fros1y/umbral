module Main where

import Prelude hiding (Either(..), id, (.))

import Game
import UI
import Level
import GameState

gameState :: GameState
gameState = mkLevel

main :: IO ()
main = do
    display <- initDisplay
    gameLoop display gameState
    endDisplay display
