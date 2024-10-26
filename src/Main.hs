module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Shoot 'Em Up" (800, 600) (0, 0)) -- Updated to 800x600
              black            -- Background color
              60               -- Frames per second for smooth movement
              Model.initialState     -- without using "Model." this caused an ambiguity error.
              view             -- View function
              input            -- Event function
              step             -- Step function

initialState :: GameState
initialState = GameState { 
                world = World 0 100 [] Nothing, -- Initial scroll position and speed
                player = Player (20, 300) 3 3 1 20 [] False, -- Player positioned near left
                score = Score 0 0,
                gamestatus = Playing,
                inputState = InputState False False False False
              }