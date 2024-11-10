module Main where

import Enemy
import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Shoot 'Em Up" (800, 600) (0, 0))
              black            -- Background color
              60               -- Frames per second for smooth movement
              Model.initialState     -- without using "Model." this caused an ambiguity error.
              view             -- View function
              input            -- Event function
              step             -- Step function
