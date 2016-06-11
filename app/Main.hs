module Main where


import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category


import Game
import Coord
import Entity
import GameState
import UI

gameState = addEntitiesToGame
                [ mkWall (Coord 2 2),
                  mkWall (Coord 4 0),
                  mkRandomRat (Coord 5 5),
                  mkZombie (Coord 10 3)]
            $ mkGameState (Coord 0 0)

main :: IO ()
main = do
  display <- initDisplay
  gameLoop display gameState
  endDisplay display
