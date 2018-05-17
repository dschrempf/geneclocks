{- |
Module      :  Geneclocks.Simulate.Coalescent
Description :  Generate coalescent trees.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Wed May 16 13:13:11 2018.

-}


module Geneclocks.Simulate.Coalescent
  ( simulate
  ) where

import Control.Monad.Primitive
import Statistics.Distribution
import Geneclocks.Distribution.CoalescentContinuous
import System.Random.MWC
import Geneclocks.Tree.Phylo

-- | Simulate a coalescent tree with 'n' leaves. The branch lengths are in units
-- of effective population size.
simulate :: (PrimMonad m)
         => Int -- ^ Number of leaves.
         -> Gen (PrimState m)
         -> m (PhyloTree Int Double)
simulate n g = simulate' n 0 trs g
  where trs = [ singleton i 0.0 | i <- [0..n-1] ]


simulate' :: (PrimMonad m)
          => Int
          -> Int
          -> [PhyloTree Int Double]
          -> Gen (PrimState m)
          -> m (PhyloTree Int Double)
simulate' n a trs g
  | n <= 0                     = error "Cannot construct trees without leaves."
  | n == 1 && length trs /= 1  = error "Too many trees provided."
  | n == 1 && length trs == 1  = return $ head trs
  | otherwise                  =
      do
        -- Indices of the leaves to join will be i-1 and i.
        i <- uniformR (1, n-1) g
        -- The time of the coalescent event.
        t <- genContVar (coalescentDistributionCont n) g
        let trs'  = map (lengthen t) trs -- Move time 't' up on the tree.
            tl    = trs' !! (i-1)
            tr    = trs' !! i
            -- Join the two chosen trees.
            tm    = glue (Info a 0.0 Internal) [tl, tr]
            -- Take the trees on the left, the merged tree, and the trees on the right.
            trs'' = take (i-1) trs' ++ [tm] ++ drop (i+1) trs'
        simulate' (n-1) a trs'' g
