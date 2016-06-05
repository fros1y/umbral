module Main where


import           Prelude              hiding (Either (..), id, (.))
import           Control.Applicative
import           Control.Category


import Game
import Coord
import Entity
import GameState

gameState = addEntitiesToGame
                [ mkWall 2 (Coord 2 2),
                  mkWall 3 (Coord 4 0),
                  mkRandomRat 4 (Coord 5 5) ]
            $ mkGameState (Coord 0 0)

main :: IO ()
main =  gameLoop gameState
