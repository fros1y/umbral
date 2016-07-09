module Main where

import Prelude hiding (Either(..), id, (.))

import Control.Lens
import Game
import UI
import LevelBuilder
import GameState

gameState :: GameState
gameState = mkLevel

main :: IO ()
main = do
    display <- initDisplay
    gameLoop (gameState & displayContext .~ Just display)
    endDisplay display
