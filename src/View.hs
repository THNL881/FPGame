
-- This module defines how to turn
-- the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Fixed (mod')
import Data.Maybe (isJust, isNothing)

view :: GameState -> IO Picture
view = return . viewPure

-- Render game elements based on game state
viewPure gstate = pictures 
    [ renderPlayer (player gstate)
    , renderRoofFloor (world gstate)
    , renderHighScore gstate
    , renderScore gstate
    , renderHP gstate
    , renderProjectileList (projectiles (player gstate))
    , renderEnemies (enemiesGame gstate)
    , renderSpecialScreen (gamestatus gstate)]

-- Renders text for when gamestatus is not playing
renderSpecialScreen :: GameStatus -> Picture
renderSpecialScreen status 
  = case status of 
     Finished 
       -> translate (-200) 0 $ scale 0.6 0.6 $ color red $ text "YOU DIED"
     Paused 
       -> translate (-170) 0 $ scale 0.6 0.6 $ color yellow $ text "PAUSED"
     _ -> Blank
    

renderDeathAnimation :: Enemy -> Picture
renderDeathAnimation enemy = case deathAnimationTimer enemy of 
  Just progress -> let (x, y) = enemyPosition enemy
                       scalar = 1 + progress * 2
                       opacity = 1 - progress
                   in translate x y $ color (makeColor 1 0 0 opacity) $ scale scalar scalar $ circleSolid 15

renderEnemies :: [Enemy] -> Picture
renderEnemies = pictures . map renderEnemy 

renderEnemy :: Enemy -> Picture
renderEnemy enemy = case enemyType enemy of 
  Kamikaze -> if isJust (deathAnimationTimer enemy) 
                then renderDeathAnimation enemy
                else renderKamikazeEnemy enemy

renderKamikazeEnemy :: Enemy -> Picture
renderKamikazeEnemy enemy = translate x y $ color white $ circleSolid 10
  where
    (x, y) = enemyPosition enemy

renderProjectileList :: [Projectile] -> Picture
renderProjectileList list = pictures (map renderSingleProjectile list)

-- Render a single projectile as a 5x5 white triangle
renderSingleProjectile :: Projectile -> Picture
renderSingleProjectile p = uncurry translate (position p) (color white $ polygon [(0, 0), (10, 2.5), (0, 5)])

renderEnemiesCount :: GameState -> Picture --remove before final
renderEnemiesCount gstate = translate 0 (-100) $ scale 0.15 0.15 $ color white $ text ("length enemy list: " ++ show (length (enemiesGame gstate))) 

-- Render the player as a triangle, changing color if damaged
renderPlayer :: Player -> Picture
renderPlayer player =
    let (x, y) = playerPosition player
        -- Set player color to red if damageTimer is active, otherwise white
        playerColor = if isJust (damageTimer player) then red else white
    in translate x y $ color playerColor $ polygon [(0, 30), (-30, -30), (30, -30)]


-- Render scrolling roof and floor
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


-- Render the high score
renderHighScore :: GameState -> Picture
renderHighScore gstate = translate (-380) 260 $ scale 0.15 0.15 $ color white $ text ("HighScore: " ++ show (highScore (score gstate)))


-- Render the current score
renderScore :: GameState -> Picture
renderScore gstate = translate (-380) 240 $ scale 0.15 0.15 $ color white $ text ("Score: " ++ show (currentScore (score gstate)))


-- Render the player's health in the top left corner
renderHP :: GameState -> Picture
renderHP gstate =
    translate (-380) 220 $ scale 0.15 0.15 $ color white $ text ("HP: " ++ show (playerHealth (player gstate)))

-- Custom color definitions
darkBlue, customBlue :: Color
darkBlue = makeColor 0 0 0.5 1
customBlue = makeColor 0 0 1 1