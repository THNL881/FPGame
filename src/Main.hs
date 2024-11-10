module Main where

import Enemy
import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
    initialState <- initializeState
    playIO (InWindow "Shoot 'Em Up" (800, 600) (0, 0))
              black            -- Background color
              60               -- Frames per second for smooth movement
              initialState     -- the initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
