-- This module handles all the logic surrounding Projectiles
module Projectiles where
  
import Model

-- projectile logic
-- Moves each projectile and removes it if out of screen bounds
updateProjectiles :: Float -> [Projectile] -> [Projectile]
updateProjectiles secs = filter inBounds . map (moveProjectile secs)
  where
    inBounds proj = fst (position proj) <= 450 -- Keep only projectiles within bounds
    
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile secs proj = proj { position = (x + speed proj * secs, y) }
  where
    (x, y) = position proj

