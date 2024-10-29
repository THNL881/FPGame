
-- | This module contains the data types
--   which represent the state of the game
module Model where


-- Information to show, for debugging or other simple displays
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5


-- | Complete game state, including player, world, score, and status
data GameState = GameState {
                   world       :: World      -- The game world (scrolling, enemies)
                 , player      :: Player     -- The player character
                 , score       :: Score      -- Score data
                 , gamestatus  :: GameStatus -- Current game status
                 , inputState  :: InputState -- Tracks current input keys
                 , elapsedTime :: Float      -- Time elapsed in seconds
                 }


-- | Game status: playing, paused, or cleared
data GameStatus = Playing | Paused | Cleared


-- | Representation of the game world
data World = World {
                   scrollPosition :: Float  -- How far the world has scrolled
                 , scrollSpeed    :: Float  -- Speed of scrolling
                 , enemies        :: [Enemy] -- List of enemies in the world
                 , boss           :: Maybe Boss -- Optional boss enemy
                 }


-- | The player character
data Player = Player {
                   playerPosition  :: Position -- Current position
                 , playerHealth    :: Int      -- Health of the player
                 , playerMaxHealth :: Int      -- Max health
                 , playerDamage    :: Int      -- Damage dealt by player
                 , playerSpeed     :: Float    -- Speed of player movement
                 , projectiles     :: [Projectile] -- Projectiles fired by player
                 , isFiring        :: Bool     -- True if player is firing
                 }


-- | Enemy character data
data Enemy = Enemy {
                   enemyType     :: EnemyType  -- Type of enemy (Shooter, Kamikaze, etc.)
                 , enemyPosition :: Position   -- Position of the enemy
                 , enemyHealth   :: Int        -- Health of the enemy
                 , enemySpeed    :: Position   -- Speed (x, y) of enemy
                 , dropHealth    :: Bool       -- Drops health if true
                 }

data EnemyType = Shooter | Kamikaze


-- | Health pickup data
data HealthPickup = HealthPickup {
                   pickupPosition :: Position -- Position of pickup
                 , isActive       :: Bool     -- True if pickup is active
                 }


-- | Projectile data
data Projectile = Projectile {
                   position :: Position -- Position of projectile
                 , speed    :: Float    -- Speed of projectile
                 }


-- | Game score data
data Score = Score {
                   currentScore :: Int -- Current score
                 , highScore    :: Int -- High score
                 }


-- | Input state tracking specific keys
data InputState = InputState {
                   moveUp    :: Bool -- 'W' key pressed
                 , moveDown  :: Bool -- 'S' key pressed
                 , shoot     :: Bool -- Spacebar to shoot
                 , pause     :: Bool -- 'P' key to pause
                 }


-- | Boss enemy data
data Boss = Boss {
                   bossHealth   :: Int       -- Boss health
                 , bossPosition :: Position  -- Boss position
                 , bossActive   :: Bool      -- True if boss is active
                 }


-- | Type for (x, y) positions
type Position = (Float, Float)


-- | Initial game state setup
initialState :: GameState
initialState = GameState {
                 world = World 0 100 [] Nothing,               -- Initial world state
                 player = Player (-360, 0) 3 3 1 20 [] False,  -- Player initial state at (-380, 0)
                 score = Score 0 0,                            -- Initial score and high score
                 gamestatus = Playing,                         -- Game starts in Playing mode
                 inputState = InputState False False False False, -- No keys pressed initially
                 elapsedTime = 0                               -- Initial elapsed time set to 0
               }
