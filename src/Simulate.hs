{- |
   Description :  Simulate trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 13 12:27:32 2018.

Different ways to simulate trees under the birth and death process.

-}

module Simulate
  ( birthDeathTree
  , birthDeathTreeRooted
  , geneFamilyTree
  )
  where

import Tree

import Control.Monad.Primitive
import System.Random.MWC
import qualified Statistics.Distribution as D (genContVar)
import qualified Statistics.Distribution.Exponential as D (exponential)
import SpeciesTree
import GeneTree

-- | Unlabeled birth death tree with height 'h', speciation rate 'l' and
-- extinction rate 'm'. The default labeling is the empty string or empty list
-- ('mempty' of Monoid). A random number generator is needed. It can be provided
-- using 'System.Random.MWC'.
birthDeathTree :: (PrimMonad m, Monoid a)
               => Double        -- ^ Tree height.
               -> Double        -- ^ Birth rate.
               -> Double        -- ^ Death rate.
               -> Gen (PrimState m) -- ^ Random number generator.
               -> m (Tree a Double)
birthDeathTree h l m g = do
  let edL = D.exponential l
      edM = D.exponential m
  lnL <- D.genContVar edL g
  lnM <- D.genContVar edM g
  -- Tree is higher than the first event.
  if (h <= lnL) && (h <= lnM)
    then return $ Origin mempty h (ExtantLeaf mempty)
    else if lnL <= lnM
         -- Speciation.
         then do t <- birthDeathTreeRooted (h - lnL) l m g
                 return $ Origin mempty lnL t
         -- Extinction
         else return $ Origin mempty lnM (ExtinctLeaf mempty)

-- | Unlabeled, rooted birth death tree with height 'h', speciation rate 'l' and
-- extinction rate 'm'. See 'birthDeathTree'.
birthDeathTreeRooted :: (PrimMonad m, Monoid a) => Double -> Double -> Double -> Gen (PrimState m) -> m (Tree a Double)
birthDeathTreeRooted h l m g = do
  (Origin _ lB lT) <- birthDeathTree h l m g
  (Origin _ rB rT) <- birthDeathTree h l m g
  return $ Node mempty lB lT rB rT

-- -- | Simulate a gene family tree for a given species tree. Without transmission
-- -- at the moment.
-- geneFamilyTree :: (PrimMonad m)
--                => STree               -- ^ Species tree.
--                -> Double              -- ^ Gene birth rate.
--                -> Double              -- ^ Gene death rate.
--                -> Gen (PrimState m)   -- ^ Random number generator.
--                -> m GTree
-- geneFamilyTree (ExtinctLeaf s) _ _ _ = return (ExtinctLeaf (mempty, s))
-- geneFamilyTree (ExtantLeaf  s) _ _ _ = return (ExtantLeaf  (mempty, s))
-- geneFamilyTree (Origin s b c)  l m g = birthDeathTree b l m g

  
-- geneFamilyTree' :: (PrimMonad m)
--                 => 
