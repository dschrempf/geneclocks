{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
   Module      :  Geneclocks.Distribution.TimeOfOrigin
   Description :  Distribution of time of origin for birth and death trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 13 13:16:18 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

Distribution of the time of origin for birth and death trees. See corollary 3.3
in the paper cited above.

-}

module Geneclocks.Distribution.TimeOfOrigin
  ( TimeOfOriginDistribution(..)
  , cumulative
  , density
  , quantile
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D
import Geneclocks.Distribution.Types

-- | Distribution of the time of origin for a phylogenetic tree evolving under
-- the birth and death process and conditioned on observing n leaves today.
data TimeOfOriginDistribution = TOD
  { todTN :: Int                -- ^ Number of leaves of the tree.
  , todLa :: BirthRate          -- ^ Birth rate.
  , todMu :: DeathRate          -- ^ Death rate.
  } deriving (Eq, Typeable, Data, Generic)

instance D.Distribution TimeOfOriginDistribution where
    cumulative = cumulative

-- | Cumulative distribution function Corollary 3.3.
cumulative :: TimeOfOriginDistribution -> Time -> Double
cumulative (TOD n l m) x
  | x <= 0    = 0
  | otherwise = te ** fromIntegral n
  where d  = l - m
        te = l * (1.0 - exp (-d*x)) / (l - m*exp(-d*x))

instance D.ContDistr TimeOfOriginDistribution where
  density  = density
  quantile = quantile

-- | The density function Eq. (5).
density :: TimeOfOriginDistribution -> Time -> Double
density (TOD nn l m) x
  | x < 0     = 0
  | otherwise = n * l**n * d**2 * t1**(n-1.0) * ex / (t2**n+1.0)
  where d  = l - m
        n  = fromIntegral nn
        ex = exp(-d*x)
        t1 = 1.0 - ex
        t2 = l - m*ex

-- | The inverted cumulative probability distribution 'cumulative'. See also
-- 'D.ContDistr'.
quantile :: TimeOfOriginDistribution -> Double -> Time
quantile (TOD nn l m) p
  | p >= 0 && p <= 1 = -1.0/d * log(t1/t2)
  | otherwise        =
    error $ "PointProcess.quantile: p must be in [0,1] range. Got: " ++ show p
 where d  = l - m
       n  = fromIntegral nn
       t1 = l*(1.0-p**(1.0/n))
       t2 = l - p**(1.0/n)*m

instance D.ContGen TimeOfOriginDistribution where
  genContVar = D.genContinuous
