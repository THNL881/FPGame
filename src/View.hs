
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Fixed (mod')

view :: GameState -> IO Picture
view = return . viewPure


-- | Render game elements based on game state
viewPure :: GameState -> Picture
viewPure gstate = pictures 
  [ renderPlayer (player gstate)
  , renderRoofFloor (world gstate)
  , renderHighScore gstate
  , renderScore gstate
  , renderHP gstate
  --, renderElapsedTime gstate
  , renderProjectileList (projectiles (player gstate))
  , renderEnemies (enemiesGame gstate)
  , renderSpawnTimer gstate
  , renderDebugString gstate
  , renderEnemiesCount gstate
  -- , renderCurrentStdGen gstate
  , renderEnemyPositions (enemiesGame gstate)]

renderEnemyPositions :: [Enemy] -> Picture
renderEnemyPositions enemies = translate (-380) (-200) $ scale 0.1 0.1 $
  color white $ text $ unlines (map (show . enemyPosition) enemies)

renderDebugString :: GameState -> Picture
renderDebugString gstate 
  = translate 0 0 $ scale 0.15 0.15 $ color white $ text ("debug string: " ++ show (debugString gstate))

renderCurrentStdGen :: GameState -> Picture
renderCurrentStdGen gstate 
  = translate (-300) (-150) $ scale 0.15 0.15 $ color white $ text ("rng: " ++ show (rng gstate))

renderEnemies :: [Enemy] -> Picture
renderEnemies = pictures . map renderEnemy 

renderEnemy :: Enemy -> Picture
renderEnemy enemy = case enemyType enemy of 
  Kamikaze -> renderKamikazeEnemy enemy

renderKamikazeEnemy :: Enemy -> Picture
renderKamikazeEnemy enemy = translate x y $ color white $ circleSolid 10
  where
    (x, y) = enemyPosition enemy

-- | Render the elapsed time
-- renderElapsedTime :: GameState -> Picture
-- renderElapsedTime gstate = translate 220 250 $ scale 0.15 0.15 $ color white $ text ("Time Elapsed: " ++ show (round (elapsedTime gstate)))

renderSpawnTimer :: GameState -> Picture
renderSpawnTimer gstate =  translate 100 100 $ scale 0.15 0.15 $ color white $ text ("spawntimer Elapsed: " ++ show (spawnTimer gstate)) 


renderProjectileList :: [Projectile] -> Picture
renderProjectileList list = pictures (map renderSingleProjectile list)


-- Render a single projectile as a 5x5 white triangle
renderSingleProjectile :: Projectile -> Picture
renderSingleProjectile p = uncurry translate (position p) (color white $ polygon [(0, 0), (10, 2.5), (0, 5)])

renderEnemiesCount :: GameState -> Picture
renderEnemiesCount gstate = translate 0 (-100) $ scale 0.15 0.15 $ color white $ text ("length enemy list: " ++ show (length (enemiesGame gstate))) 

-- | Render the player as a red triangle
renderPlayer :: Player -> Picture
renderPlayer player = translate x y $ color red $ polygon [(0, 30), (-30, -30), (30, -30)]
  where
    (x, y) = playerPosition player


-- | Render scrolling roof and floor
renderRoofFloor :: World -> Picture
renderRoofFloor world = pictures [roof, floor]
  where
    -- Function to create a repeating strip of alternating colors
    strip offset = pictures [translate (fromIntegral i - offset - 400) 0 singleStrip | i <- [0,80..windowWidth + 80]] -- -400 is the start of the window
      where
        singleStrip = pictures $ concatMap (\i ->
          [color darkBlue $ rectangleSolid 40 20, translate 40 0 $ color customBlue $ rectangleSolid 40 20]) [0,80..windowWidth]
    
    scrollPos = scrollPosition world `mod'` 80
    roof = translate 0 290 $ strip scrollPos  -- Roof positioned at top
    floor = translate 0 (-290) $ strip scrollPos  -- Floor positioned at bottom

    windowWidth = 800


-- | Render the high score
renderHighScore :: GameState -> Picture
renderHighScore gstate = translate (-380) 260 $ scale 0.15 0.15 $ color white $ text ("HighScore: " ++ show (highScore (score gstate)))


-- | Render the current score
renderScore :: GameState -> Picture
renderScore gstate = translate (-380) 240 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show (currentScore (score gstate)))


-- | Render the player's health
renderHP :: GameState -> Picture
renderHP gstate = translate (-380) (-275) $ scale 0.15 0.15 $ color white $ text ("HP: " ++ show (playerHealth (player gstate)))


-- Custom color definitions
darkBlue, customBlue :: Color
darkBlue = makeColor 0 0 0.5 1
customBlue = makeColor 0 0 1 1