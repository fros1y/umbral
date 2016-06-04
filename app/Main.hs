module Main where

import Game
import Coord

gameState = addEntitiesToGame
                [ mkWall 2 (Coord 2 2),
                  mkWall 3 (Coord 4 0),
                  mkRandomRat 4 (Coord 5 5) ]
            $ mkGameState (Coord 0 0)

main :: IO ()
main =  gameLoop gameState
