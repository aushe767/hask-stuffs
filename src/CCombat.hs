module CCombat where

import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time
import Control.Concurrent

-- First, let's define our core types for FRP
type Time = NominalDiffTime
type Behavior a = Time -> a
type Event a = [(Time, a)]

data CombatGuy = CombatGuy
  { hp :: Float,
    def :: Float,
    att :: Float,
    att_multiplier :: Float
  } deriving (Show, Eq)

mycombatguy = CombatGuy 100 20 40 1.0

--- >>> :t mycombatguy
-- mycombatguy :: CombatGuy

combatguy2 = CombatGuy 100 21 41 1.1

-- Combat-specific types
data AttackAction = AttackAction
  { attacker :: CombatGuy
  , target :: CombatGuy
  , timestamp :: Time
  } deriving Show

data CombatState = CombatState
  { player :: CombatGuy
  , enemy :: CombatGuy
  , lastAttackTime :: Time
  , cooldown :: Time  -- time required between attacks
  } deriving Show

-- Calculate damage for an attack
calculateDamage :: CombatGuy -> CombatGuy -> Float
calculateDamage attacker defender =
    let rawDamage = att attacker * att_multiplier attacker
        reduction = def defender / 100.0
    in rawDamage * (1 - reduction)

-- Behavior that tracks combat state over time
combatBehavior :: Event AttackAction -> Behavior CombatState
combatBehavior attacks t = 
    let initialState = CombatState mycombatguy combatguy2 0 1.0
        -- Get all attacks that happened before current time
        relevantAttacks = filter (\(time, _) -> time <= t) attacks
    in foldr applyAttack initialState relevantAttacks
  where
    applyAttack (time, action) state =
        if time - lastAttackTime state >= cooldown state
        then 
            -- Apply damage to target
            let damage = calculateDamage (attacker action) (target action)
                updatedTarget = (target action) { hp = hp (target action) - damage }
                -- Update state based on who was attacked
                newState = if target action == player state
                          then state { player = updatedTarget }
                          else state { enemy = updatedTarget }
            in newState { lastAttackTime = time }
        else state

-- Example game loop using these FRP concepts
gameLoop :: IO ()
gameLoop = do
    -- Initialize attack events list
    let attackEvents = [] :: Event AttackAction
    
    -- Start game loop
    startTime <- getCurrentTime
    gameLoopWithEvents startTime attackEvents

gameLoopWithEvents :: UTCTime -> Event AttackAction -> IO ()
gameLoopWithEvents startTime events = do
    now <- getCurrentTime
    let currentTime = realToFrac $ diffUTCTime now startTime

    -- Get current combat state
    let state = combatBehavior events currentTime
    
    -- Check for player input (simplified)
    userInput <- getLine
    let newEvents = case userInput of
          "attack" -> events ++ [(currentTime, AttackAction mycombatguy combatguy2 currentTime)]
          _ -> events
    
    -- Print current state
    print state
    
    -- Continue loop with updated events
    threadDelay 16666  -- ~60 FPS
    gameLoopWithEvents startTime newEvents

-- Helper function to create an attack event
attack :: CombatGuy -> CombatGuy -> Time -> AttackAction
attack from to t = AttackAction from to t

-- Example of querying state at different times
queryExampleCombat :: IO ()
queryExampleCombat = do
    -- Create a sequence of attack events
    let sampleEvents = 
          [ (0.0, attack mycombatguy combatguy2 0.0)
          , (1.5, attack combatguy2 mycombatguy 1.5)
          , (3.0, attack mycombatguy combatguy2 3.0)
          ]
    
    -- Query state at different times
    let t1 = 0.5  -- after first attack
    let t2 = 2.0  -- after second attack
    let t3 = 3.5  -- after all attacks
    
    putStrLn $ "State at t=0.5: " ++ show (combatBehavior sampleEvents t1)
    putStrLn $ "State at t=2.0: " ++ show (combatBehavior sampleEvents t2)
    putStrLn $ "State at t=3.5: " ++ show (combatBehavior sampleEvents t3)
