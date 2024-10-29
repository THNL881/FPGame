
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  let updatedProjectiles = updateProjectiles secs (projectiles (player gstate))
  let updatedPlayer = (player gstate) { projectiles = updatedProjectiles }
  let newGState = gstate { 
                  player = updatedPlayer,
                  world = (world gstate) { scrollPosition = scrollPosition (world gstate) + scrollSpeed (world gstate) * secs },
                  elapsedTime = elapsedTime gstate + secs
                 }
  return newGState



--projectile logic
-- Moves each projectile and removes it if out of screen bounds
updateProjectiles :: Float -> [Projectile] -> [Projectile]
updateProjectiles secs = filter inBounds . map (moveProjectile secs)
  where
    inBounds proj = fst (position proj) <= 450 -- Keep only projectiles within bounds

moveProjectile :: Float -> Projectile -> Projectile
moveProjectile secs proj = proj { position = (x + speed proj * secs, y) }
  where
    (x, y) = position proj


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = do
  let inputState' = inputKey e (inputState gstate)
  let newPlayer = movePlayer inputState' (shootPlayer inputState' (player gstate))
  let newGState = gstate { player = newPlayer, inputState = inputState' }
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
  | moveUp input   = p { playerPosition = clampPosition (x, y + playerSpeed p) } -- `W` key should move up
  | moveDown input = p { playerPosition = clampPosition (x, y - playerSpeed p) } -- `S` key should move down
  | otherwise      = p
  where
    (x, y) = playerPosition p
    -- Updated bounds for the player’s vertical movement
    clampPosition (px, py) = (px, max (-250) (min 250 py))  -- Constrain y between -250 and 250


shootPlayer :: InputState -> Player -> Player
shootPlayer input p
 | shoot input && not (isFiring p) && length (projectiles p) < 150 = 
     p { isFiring = True, projectiles = newProjectile : projectiles p }
 | not (shoot input) = p { isFiring = False }
 | otherwise = p
 where 
   -- Make sure the projectile starts at the player's actual position
   newProjectile = Projectile { position = playerPosition p, speed = 80 }


