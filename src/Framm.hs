{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Framm where

import qualified Control.Foldl as L
import Data.Vinyl.Curry (runcurryX)
import Frames

-- the following tabletypes thing is kind of like a compile time function
-- or MACRO

-- | Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "Prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "Prestige.csv")

-- quasi quotes working with template haskell 'Income and Prestige dont really exist until macro works

-- | Compute the ratio of income to prestige for a record containing only those fields.
ratio :: Record '[Income, Prestige] -> Double
ratio = runcurryX (\i p -> fromIntegral i / p)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) avg) <$> loadRows
  where
    avg = (/) <$> L.sum <*> L.genericLength
