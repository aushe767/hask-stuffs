module ReactifCalc where

import Data.Maybe
import Reactive.Banana hiding (filterE)

filterE p = filterJust . fmap (\e -> if p e then Just e else Nothing)
filter1   = filterE (>= 3)
filter2   = filterE (>= 3) . fmap (subtract 1)

--- >>> filterE
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')
