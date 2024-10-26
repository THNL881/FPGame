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
viewPure gstate = pictures [renderPlayer (player gstate), renderRoofFloor (world gstate), renderTexts gstate] --renders the things within the list

-- | Render the player as a red triangle
renderPlayer :: Player -> Picture
renderPlayer player = translate (-350) (300 - y) $ color red $ polygon [(0, 30), (-30, -30), (30, -30)]
  where
    (_, y) = playerPosition player  -- Set x to 20, only change y based on input

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

-- | Render the high score, current score, and player health
renderTexts :: GameState -> Picture
renderTexts gstate = pictures [highScoreText, scoreText, hpText]
  where
    highScoreText = translate (-380) 275 $ scale 0.5 0.5 $ color white $ text ("High Score: " ++ show (highScore gstate))
    scoreText = translate (-380) 260 $ scale 0.5 0.5 $ color white $ text ("Score: " ++ show (currentScore (score gstate)))
    hpText = translate (-380) (-275) $ scale 0.5 0.5 $ color white $ text ("HP: " ++ show (playerHealth (player gstate)))


-- Custom color definitions
darkBlue, customBlue :: Color
darkBlue = makeColor 0 0 0.5 1
customBlue = makeColor 0 0 1 1