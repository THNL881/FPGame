
-- This module contains the data types
-- which represent the state of the game
module Model where
import System.Random (StdGen, mkStdGen)
import Control.Exception(catch, IOException)
{-
-- Information to show, for debugging or other simple displays
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar Char -}

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

-- Complete game state, including player, world, score, and status
data GameState = GameState {
                   world            :: World      -- The game world (scrolling, enemies)
                 , player           :: Player     -- The player character
                 , score            :: Score      -- Score data
                 , gamestatus       :: GameStatus -- Current game status
                 , inputState       :: InputState -- Tracks current input keys
                 , elapsedTime      :: Float      -- Time elapsed in seconds
                 , spawnTimer       :: Float      -- Timer that tracks shooting enemies interval
                 , enemiesGame      :: [Enemy]    -- Enemies in the game
                 , rng              :: StdGen     -- Pure randomness
                 }


-- Game status: playing, paused, or cleared
data GameStatus = Playing | Paused | Finished


-- Representation of the game world
data World = World {
                   scrollPosition :: Float  -- How far the world has scrolled
                 , scrollSpeed    :: Float  -- Speed of scrolling
                 , enemies        :: [Enemy] -- List of enemies in the world
                 }


-- The player character
data Player = Player {
                   playerPosition  :: Position      -- Current position
                 , playerHealth    :: Int           -- Health of the player
                 , playerMaxHealth :: Int           -- Max health
                 , playerDamage    :: Int           -- Damage dealt by player
                 , playerSpeed     :: Float         -- Speed of player movement
                 , projectiles     :: [Projectile]  -- Projectiles fired by player
                 , isFiring        :: Bool          -- True if player is firing
                 , damageTimer     :: Maybe Float
                 }


-- Enemy character data
data Enemy = Enemy {
                   enemyType           :: EnemyType        -- Type of enemy (Shooter, Kamikaze, etc.)
                 , enemyPosition       :: Position         -- Position of the enemy
                 , enemyHealth         :: Int              -- Health of the enemy
                 , enemySpeed          :: Position         -- Speed (x, y) of enemy
                 , isDead              :: Bool             -- Drops health if true
                 , deathAnimationTimer :: Maybe Float      -- times the animation when an enemy dies
                 }
              

data EnemyType = Kamikaze --originally used for possibility of multiple enemies

-- Projectile data
data Projectile = Projectile {
                   position :: Position -- Position of projectile
                 , speed    :: Float    -- Speed of projectile
                 }


-- Game score data
data Score = Score {
                   currentScore :: Int -- Current score
                 , highScore    :: Int -- High score
                 }


-- Input state tracking specific keys
data InputState = InputState {
                   moveUp    :: Bool -- 'W' key pressed
                 , moveDown  :: Bool -- 'S' key pressed
                 , shoot     :: Bool -- Spacebar to shoot
                 , pause     :: Bool -- 'P' key to pause
                 }

-- | Type for (x, y) positions
type Position = (Float, Float)


loadHighScore :: FilePath -> IO Int
loadHighScore filePath = do
    contents <- catch (readFile filePath) handleReadError
    let scores = map read (lines contents)
    return $ if null scores then 0 else maximum scores
  where  
    handleReadError :: IOException -> IO String
    handleReadError _ = return "0"  -- Return 0 if file does not exist -> initializing the game


-- | Initial game state setup
initializeState :: IO GameState
initializeState = do
  highScore <-loadHighScore "Scores.txt"
  return GameState {
                 world       = World 0 100 [],                                  -- Initial world state
                 player      = Player (-360, 0) 3 3 1 20 [] False Nothing,      -- Player initial state at (-380, 0)
                 score       = Score 0 highScore,                               -- Initial score and high score
                 gamestatus  = Playing,                                         -- Game starts in Playing mode
                 inputState  = InputState False False False False,              -- No keys pressed initially
                 elapsedTime = 0,                                               -- Initial elapsed time set to 0
                 spawnTimer  = 0,                                               -- spawntimer set to 0 initially
                 enemiesGame = [],                                              -- 0 enemies when the game starts
                 rng         = mkStdGen 42                                      -- rng for enemy spawns
               }
