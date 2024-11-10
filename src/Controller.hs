-- This module defines how the state changes
-- in response to time and user input
module Controller where

--imports
import Projectiles 
import Player
import Enemy
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
    ( Event(EventKey),
      Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeySpace) )
import System.Random
import Data.Maybe ( isNothing, mapMaybe )
import Debug.Trace
import System.Exit (exitSuccess)
import Data.List (partition)

-- Handle one iteration of the game 

step :: Float -> GameState -> IO GameState
step secs gstate = do
  case gamestatus gstate of
    Playing -> updateGame secs gstate
    Paused  -> pauseGame gstate
    Finished -> endGame gstate

pauseGame :: GameState -> IO GameState
pauseGame gstate = return gstate { gamestatus = updateStatus gstate }

updateGame :: Float -> GameState -> IO GameState
updateGame secs gstate = do
    let gstateWithCollisions = checkPlayerCollisions gstate
        newSpawnTimer = spawnTimer gstateWithCollisions + secs
        (gstateNew, resetTimer) = 
            if newSpawnTimer >= 0.6
                then (spawnKamikazeEnemy gstateWithCollisions, 0) 
                else (gstateWithCollisions, newSpawnTimer)

        updatedEnemies     = updateEnemies secs gstateNew (enemiesGame gstateNew)
        updatedProjectiles = updateProjectiles secs (projectiles (player gstateNew))
        updatedPlayer      = (player gstateNew) { 
            projectiles = updatedProjectiles,
            damageTimer = case damageTimer (player gstateNew) of
                            Just t -> if t - secs <= 0 then Nothing else Just (t - secs)
                            Nothing -> Nothing
        }
        defeatedEnemies    = length (enemiesGame gstateNew) - length updatedEnemies
        updatedScore       = (score gstateNew) { currentScore = currentScore (score gstateNew) + 5 * defeatedEnemies }
        updatedStatus      = updateStatus gstateNew

        newGState = gstateNew { 
            player      = updatedPlayer,
            score       = updatedScore,
            gamestatus  = updatedStatus,
            spawnTimer  = resetTimer,
            world       = (world gstate) { scrollPosition = scrollPosition (world gstate) + scrollSpeed (world gstate) * secs },
            elapsedTime = elapsedTime gstate + secs,
            enemiesGame = updatedEnemies,
            rng         = rng gstateNew
        } 
    return newGState

-- Ends the game
endGame :: GameState -> IO GameState
endGame gstate = do
  let clearedGstate = gstate { gamestatus = Finished }
  return clearedGstate
 
-- Updates the gamestatus 
updateStatus :: GameState -> GameStatus
updateStatus gstate | playerHealth (player gstate) <= 0 = Finished
                    | pause (inputState gstate)         = Paused
                    | otherwise                         = Playing
    
-- Write score to a standard filePath
writeScoreToFile :: FilePath -> Int -> IO ()
writeScoreToFile filePath score = do
    appendFile filePath scoreString
      where 
        scoreString = show score ++ "\n"

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = 
  case gamestatus gstate of 
     Paused -> --Pressing P will toggle pause
      case e of
        EventKey (Char 'p') Down _ _ -> do
          let inputState' = inputKey e (inputState gstate)
          let newGState = gstate { inputState = inputState', gamestatus = Playing }
          return newGState
        _ -> return gstate  -- Ignore all other inputs while paused
      
     Playing -> do
      let inputState' = inputKey e (inputState gstate)
      let newPlayer   = movePlayer inputState' (shootPlayer inputState' (player gstate))
      let newGState   = gstate { player = newPlayer, inputState = inputState' }
      return newGState 
      
     Finished -> 
      case e of -- Press l to exit the game and save the score
        EventKey (Char 'l') Down _ _ -> do
          writeScoreToFile "Scores.txt" (currentScore (score gstate))
          exitSuccess
        _                            -> return gstate


-- Detect key presses and update input state
inputKey :: Event -> InputState -> InputState
inputKey (EventKey (Char 'p') Down _ _) is            = is { pause = not (pause is) }
inputKey (EventKey (Char 'w') Down _ _) is            = is { moveUp = True }
inputKey (EventKey (Char 'w') Up _ _)   is            = is { moveUp = False }
inputKey (EventKey (Char 's') Down _ _) is            = is { moveDown = True }
inputKey (EventKey (Char 's') Up _ _)   is            = is { moveDown = False }
inputKey (EventKey (SpecialKey KeySpace) Down _ _) is = is { shoot = True }
inputKey (EventKey (SpecialKey KeySpace) Up _ _) is   = is { shoot = False }
inputKey _ is = is

-- Check for collisions between the player and kamikaze enemies
checkPlayerCollisions :: GameState -> GameState
checkPlayerCollisions gstate =
    let (collidingEnemies, remainingEnemies) = partition (isPlayerCollision gstate) (enemiesGame gstate)
        playerHit = not (null collidingEnemies)
        updatedPlayer = if playerHit
                        then (player gstate) { playerHealth = max 0 (playerHealth (player gstate) - 1),
                                               damageTimer = Just 0.5 } -- Set damage timer for 0.5s
                        else player gstate
    in gstate { player = updatedPlayer, enemiesGame = remainingEnemies }  -- Remove colliding enemies

-- Check if an enemy is colliding with the player
isPlayerCollision :: GameState -> Enemy -> Bool
isPlayerCollision gstate enemy =
    let (px, py) = playerPosition (player gstate)
        (ex, ey) = enemyPosition enemy
        collisionRadius = 15 -- Adjust as needed for collision size
    in abs (px - ex) < collisionRadius && abs (py - ey) < collisionRadius





