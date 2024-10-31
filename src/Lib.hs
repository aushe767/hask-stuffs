module Lib
  ( someFunc,
  )
where

someFunc = putStrLn "someFunc"

data CombatGuy = CombatGuy
  { hp :: Float,
    def :: Float,
    att :: Float,
    att_multiplier :: Float
  } deriving Show

mycombatguy = CombatGuy 100 20 40 1.0

--- >>> :t mycombatguy
-- mycombatguy :: CombatGuy

combatguy2 = CombatGuy 100 21 41 1.1

--  listen on to guy
