{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

{- |
   Module      :  Geneclocks.Distribution.BirthDeathCritical
   Description :  Birth and death distribution
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 13 13:16:18 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

Distribution of the values of the point process such that it corresponds to
reconstructed trees under the birth and death process; critical birth and death
process with lambda=mu.

-}

module Geneclocks.Distribution.BirthDeathCritical
  ( BirthDeathCriticalDistribution(..)
  , cumulative
  , density
  , quantile
  ) where

import           Data.Data                     (Data, Typeable)
import           Geneclocks.Distribution.Types
import           GHC.Generics                  (Generic)
import qualified Statistics.Distribution       as D

-- | Distribution of the values of the point process such that it corresponds to
-- a reconstructed tree of the birth and death process.
data BirthDeathCriticalDistribution = BDCD
  { bdcdTOr :: Time         -- ^ Time to origin of the tree.
  , bdcdLa  :: Rate    -- ^ Birth and death rate.
  } deriving (Eq, Typeable, Data, Generic)

instance D.Distribution BirthDeathCriticalDistribution where
    cumulative = cumulative

-- | Cumulative distribution function section 2.1.2, second formula.
cumulative :: BirthDeathCriticalDistribution -> Time -> Double
cumulative (BDCD t l) x
  | x <= 0    = 0
  | x >  t    = 1
  | otherwise = x / (1.0 + l * x) * (1.0 + l * t) / t

instance D.ContDistr BirthDeathCriticalDistribution where
  density  = density
  quantile = quantile

-- | Density function section 2.1.2, first formula.
density :: BirthDeathCriticalDistribution -> Time -> Double
density (BDCD t l) x
  | x < 0     = 0
  | x > t     = 0
  | otherwise = (1.0 + l * t) / (t * (1.0 + l * x)**2)

-- | Inverted cumulative probability distribution 'cumulative'. See also
-- 'D.ContDistr'.
quantile :: BirthDeathCriticalDistribution -> Double -> Time
quantile (BDCD t l) p
  | p >= 0 && p <= 1 = res
  | otherwise        =
    error $ "PointProcess.quantile: p must be in [0,1] range. Got: " ++ show p
 where res = p * t / (1 + l*t - l*p*t)

instance D.ContGen BirthDeathCriticalDistribution where
  genContVar = D.genContinuous
