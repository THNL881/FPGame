 -- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

--hieronder de originele step functie van de template
{- 
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs } 
-}

--nieuwe step functie
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate { world = (world gstate) { scrollPosition = scrollPosition (world gstate) + scrollSpeed (world gstate) * secs } }

-- | Handle user input
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return $ gstate { player = movePlayer (inputKey e (inputState gstate)) (player gstate) } 
--this only changes the player in the gamestate for now, not the rest

-- | Detect key presses and update input state
inputKey :: Event -> InputState -> InputState
inputKey (EventKey (Char 'w') Down _ _) is = is { moveUp = True }
inputKey (EventKey (Char 'w') Up _ _)   is = is { moveUp = False }
inputKey (EventKey (Char 's') Down _ _) is = is { moveDown = True }
inputKey (EventKey (Char 's') Up _ _)   is = is { moveDown = False }
inputKey _ is = is

--this was the implementation of the template (might still be useful later)
{-
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same
-}

-- | Move player based on input state, clamping between roof and floor
movePlayer :: InputState -> Player -> Player
movePlayer input p
  | moveUp input   = p { playerPosition = clampPosition (x, y - playerSpeed p) }
  | moveDown input = p { playerPosition = clampPosition (x, y + playerSpeed p) }
  | otherwise      = p
  where
    (x, y) = playerPosition p
    clampPosition (px, py) = (px, max 50 (min 550 py))  -- Constrain between 40 and 560 so that the player can't go out of bounds.