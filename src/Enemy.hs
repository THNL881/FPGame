-- This module handles enemy logic 
-- that has nothing to do with IO (so it is pure)

module Enemy where
import Model
import System.Random (Random(randomR))

moveEnemy :: Float -> GameState -> Enemy -> Enemy
moveEnemy secs gstate enemy = enemy { enemyPosition = (x + enemySpeedX * secs, y + enemySpeedY) }
  where (x, y) = enemyPosition enemy
        (enemySpeedX, _) = enemySpeed enemy
        enemySpeedY | snd (playerPosition (player gstate)) > y = 0.3
                    | snd (playerPosition (player gstate)) < y = -0.3
                    | otherwise = y

spawnKamikazeEnemy :: GameState -> GameState 
spawnKamikazeEnemy gstate = gstate { enemiesGame = newEnemy : enemiesGame gstate, rng = newRng}
  where newEnemy = Enemy { enemyType = Kamikaze
                         , enemyPosition = (250, randomY)
                         , enemyHealth = 3
                         , enemySpeed = (-110, 0)
                         , isDead = False
                         , deathAnimationTimer = Nothing
                        }
        (randomY, newRng) = randomR (-260, 260) (rng gstate)  -- Generate random Y and update generator


-- Check if a projectile and an enemy are colliding
isCollision :: Projectile -> Enemy -> Bool
isCollision proj enemy = abs (px - ex) < 10 && abs (py - ey) < 10
  where
    (px, py) = position proj
    (ex, ey) = enemyPosition enemy

-- enemy logic and update function
updateEnemies :: Float -> GameState -> [Enemy] -> [Enemy]
updateEnemies secs gstate = filter alive . map (updateEnemy secs gstate)
  where 
    updateEnemy secs gstate enemy
      | enemyHealth enemy <= 0 = checkDeath enemy  -- Start death animation when health <= 0
      | otherwise = 
          let 
              enemy' = moveEnemy secs gstate enemy
              updatedEnemy = if isHit enemy' (projectiles (player gstate))
                             then enemy' { enemyHealth = enemyHealth enemy' - 1 }
                             else enemy'
          in if enemyHealth updatedEnemy <= 0
             then checkDeath updatedEnemy  -- Enemy is defeated, remove it
             else updatedEnemy

    -- Function to trigger the death animation or mark the enemy as dead
    checkDeath enemy = case deathAnimationTimer enemy of 
      Just progress | progress < 10 -> enemy { deathAnimationTimer = Just (progress + 0.1)} 
      Just _                        -> enemy { isDead = True }
      Nothing                       -> enemy { deathAnimationTimer = Just 0 }

    -- Function to check if an enemy is still within the game bounds
    inBounds enemy = fst (enemyPosition enemy) >= -500
    
    -- Check if enemy is still alive (not dead)
    alive enemy = not (isDead enemy)
    
    -- Function to check if an enemy is hit by any projectile
    isHit enemy projectiles = any (`isCollision` enemy) projectiles

