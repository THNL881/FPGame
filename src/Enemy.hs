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
              (enemyHit, remainingProjectiles) = checkCollision enemy' (projectiles (player gstate))
              updatedEnemy = if enemyHit then enemy' { enemyHealth = enemyHealth enemy' - 1 } else enemy'
          in if enemyHit && enemyHealth updatedEnemy <= 0
             then checkDeath updatedEnemy  -- Enemy is defeated, remove it
             else updatedEnemy
      where
        -- Check for collision and filter out hit projectiles (this doesn't work sadly)  
        checkCollision enemy = foldr (\proj (hit, projs) ->
          if isCollision proj enemy then (True, projs) else (hit, proj : projs)) (False, [])

    checkDeath enemy = case deathAnimationTimer enemy of 
      Just progress | progress < 10 -> enemy { deathAnimationTimer = Just (progress + 0.1)} 
      Just _                        -> enemy { isDead = True }
      Nothing                       -> enemy { deathAnimationTimer = Just 0 }

    inBounds enemy = fst (enemyPosition enemy) >= -500
    alive enemy = not (isDead enemy)
