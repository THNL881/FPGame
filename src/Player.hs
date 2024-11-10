-- This module handles all player logic

module Player where

import Model
-- Move player based on input state, clamping between roof and floor
movePlayer :: InputState -> Player -> Player
movePlayer input p
  | moveUp input   = p { playerPosition = clampPosition (x, y + playerSpeed p) } -- 'W' key should move up
  | moveDown input = p { playerPosition = clampPosition (x, y - playerSpeed p) } -- 'S' key should move down
  | otherwise      = p
  where
    (x, y) = playerPosition p
    -- bounds for the player’s vertical movement
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

