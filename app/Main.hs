{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Arrow
import Control.Category
import Lib
import Prelude hiding (id, (.))

main = someFunc

--- >>> someFunc

newtype SimpleFunc a b = SimpleFunc
  { runF :: a -> b
  }

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f)
    where
      mapFst g (a, b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
    where
      mapSnd g (a, b) = (a, g b)

instance Category SimpleFunc where
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
  id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x, x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

f *** g = first f >>> second g

f &&& g = split >>> first f >>> second g

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

-- ambiguous without type, arr refers to Arrow a
f :: SimpleFunc Int Int
f = arr (`div` 2)

--- >>> first f
-- No instance for `Show
--                    (SimpleFunc (Int, d0_aXM2[tau:0]) (Int, d0_aXM2[tau:0]))'
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_aXKh


g :: SimpleFunc Int Int
g = arr (\x -> x *3 + 1)

h :: SimpleFunc Int Int
h = Main.liftA2 (+) f g

hOutput = runF h 8

--- >>> hOutput
-- 29
