
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  let updatedProjectiles = map (moveProjectile secs) (projectiles (player gstate))
  let updatedPlayer = (player gstate) { projectiles = updatedProjectiles }
  let newGState = gstate { world = (world gstate) { scrollPosition = scrollPosition (world gstate) + scrollSpeed (world gstate) * secs } }
  -- Add logic here to update the score based on game events
  -- For example, increase score when an enemy is defeated
  return newGState

--projectile logic
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile secs proj = proj { position = (x + speed proj * secs, y) }
  where
    (x, y) = position proj

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = do
  let newGState = gstate { player = movePlayer (inputKey e (inputState gstate)) (player gstate) }
  case gamestatus newGState of
    Cleared -> do
      let newHighScore = max (currentScore (score newGState)) (highScore (score newGState))
      return $ newGState { score = Score (currentScore (score newGState)) newHighScore }
    _ -> return newGState


-- | Detect key presses and update input state
inputKey :: Event -> InputState -> InputState
inputKey (EventKey (Char 'w') Down _ _) is            = is { moveUp = True }
inputKey (EventKey (Char 'w') Up _ _)   is            = is { moveUp = False }
inputKey (EventKey (Char 's') Down _ _) is            = is { moveDown = True }
inputKey (EventKey (Char 's') Up _ _)   is            = is { moveDown = False }
inputKey (EventKey (SpecialKey KeySpace) Down _ _) is = is { shoot = True }
inputKey (EventKey (SpecialKey KeySpace) Up _ _) is   = is { shoot = False }
inputKey _ is = is


-- | Move player based on input state, clamping between roof and floor
movePlayer :: InputState -> Player -> Player
movePlayer input p
  | moveUp input   = p { playerPosition = clampPosition (x, y - playerSpeed p) }
  | moveDown input = p { playerPosition = clampPosition (x, y + playerSpeed p) }
  | otherwise      = p
  where
    (x, y) = playerPosition p
    clampPosition (px, py) = (px, max 50 (min 550 py))  -- Constrain between 40 and 560 so that the player can't go out of bounds.

shootPlayer :: InputState -> Player -> Player
shootPlayer input p 
 | shoot input && not (isFiring p) = p { isFiring = True, projectiles = newProjectile : projectiles p }
 | not (shoot input)               = p { isFiring = False }
 | otherwise                       = p 
 where newProjectile = Projectile { position = playerPosition p, speed = 5 }

