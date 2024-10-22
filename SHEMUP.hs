

--Data types of the Game

data GameState = GameState 
	{ world :: World
	, player :: Player
	, score :: Score
	, gamestatus :: Gamestatus
	, inputState :: ImputState }

type Position = (Float, Float)
data World = World
    { scrollPosition :: Float          -- How far the world has scrolled
    , scrollSpeed    :: Float          -- The speed at which the world scrolls
    , levelLayout    :: LevelLayout    -- Predefined level structure (obstacles, triggers)
    , enemies        :: [Enemy]        -- Active enemies in the world
    , boss           :: Maybe Boss     -- Optional boss (Nothing if not yet reached)
    }

Enemies
data EnemyType = Shooter | kamikaze | Boss
data Enemy = Enemy
    { enemyType       :: EnemyType
    , enemyPosition  :: Position
    , enemyHealth    :: Int
    , enemySpeed    :: Position
    , dropHealth       :: Bool  -- If true then the enemy should drop a health pickup upon dying, determined by random algorithm.
    }

data HealthPickup = HealthPickup 
   { pickupPosition :: Position
   , isActive :: Bool }

data Player = Player
    { playerPosition  :: Position
    , playerHealth    :: Int – keep track of current player health
    , playerMaxHealth :: Int – likely set to 3
    , playerDamage    :: Int  --  likely set to 1
    , playerSpeed :: Int – rate at which the player moves due to a ‘w’ or ‘s’ press
    , projectiles     :: [Projectile] – is this useful or necessary?
    , isFiring :: Bool – if true then projectiles will be fired, otherwise nothing should be done
    }

data Score = Score
    { currentScore :: Int    -- The player's current score
    , highScore    :: Int    -- The high score
    }

data Score = Score
    { currentScore :: Int    -- The player's current score
    , highScore    :: Int    -- The high score
    }

willDrop :: Bool
willDrop = if (random x > 80) then True else False

data InputState = InputState
    { moveUp    :: Bool    -- 'W' key
    , moveDown  :: Bool    -- 'S' key
    , shoot     :: Bool    -- Spacebar to shoot
    , pause     :: Bool    -- 'P' key to pause
    }

data Boss = Boss
    { bossHealth    :: Int
    , bossPosition  :: Position 
    , bossActive    :: Bool
    }






