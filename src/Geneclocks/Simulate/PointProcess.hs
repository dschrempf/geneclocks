{-# LANGUAGE BangPatterns #-}

{- |
   Module      :  Geneclocks.Simulate.PointProcess
   Description :  Point process and functions
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 13 13:16:18 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

The point process can be used to simulate reconstructed trees under the birth
and death process.

-}

module Geneclocks.Simulate.PointProcess
  ( PointProcess(..)
  , simulate
  , toReconstructedTree
  , simulateReconstructedTree
  , simulateReconstructedTreeRandomHeight
  -- , toBranchLengthNChildren
  -- , simulateBranchLengthNChildren
  -- , simulateBranchLengthNChildrenRandomHeight
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.List                            (mapAccumL)
import           Geneclocks.Distribution.BirthDeath
import           Geneclocks.Distribution.TimeOfOrigin
import           Geneclocks.Distribution.Types
import           Geneclocks.Tools
import           Geneclocks.Tree.Phylo
import qualified Statistics.Distribution              as D (genContVar)
import           System.Random.MWC

-- | A __point process__ for \(n\) points and of age \(t_{or}\) is defined as
-- follows. Draw $n$ points on the horizontal axis at \(1,2,\ldots,n\). Pick
-- \(n-1\) points at locations \((i+1/2, s_i)\), \(i=1,2,\ldots,n-1\);
-- \(0 < s_i < t_{or}\). There is a bijection between (ranked) oriented trees
-- and the point process. Usually, a will be 'String' (or 'Int') and b will be
-- 'Double'.
data PointProcess a b = PointProcess
  { points :: ![a]
  , values :: ![b]
  , origin :: !b } deriving (Read, Show, Eq)

-- | Sample a point process using the 'BirthDeathDistribution'. The names of the
-- points will be integers.
simulate :: (PrimMonad m)
         => Int             -- ^ Number of points (samples)
         -> Time            -- ^ Time of origin
         -> BirthRate       -- ^ Birth rate
         -> DeathRate       -- ^ Death rate
         -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
         -> m (PointProcess Int Double)
simulate n t l m g
  | n < 1     = error "Number of samples needs to be one or larger."
  | t < 0.0   = error "Time of origin needs to be positive."
  | l < 0.0   = error "Birth rate needs to be positive."
  | m < 0.0   = error "Death rate needs to be positive."
  | otherwise = do
  !vs <- replicateM (n-1) (D.genContVar (BDD t l m) g)
  return $ PointProcess [0..(n-1)] vs t

-- | Sort the values of a point process and their indices to be (the indices
-- that they will have while creating the tree).
sort :: (Ord b) => PointProcess a b -> ([b], [Int])
sort (PointProcess _ vs _) = (vsSorted, isSorted)
  where vsIsSorted = sortWithIndices vs
        vsSorted = map fst vsIsSorted
        isSorted = flattenIndices $ map snd vsIsSorted

-- Decrement indices that are above the one that is merged.
flattenIndices :: [Int] -> [Int]
flattenIndices is = snd $ mapAccumL fAcc [] is

-- The accumulating function. Count the number of indices which are before the
-- current index and lower than the current index.
fAcc :: [Int] -> Int -> ([Int], Int)
fAcc is i = (i:is, i')
  where i' = i - length (filter (<i) is)

-- | Same as 'simulateReconstructedTree' but tree height is drawn from the
-- expected distribution. See 'TOD.TimeOfOriginDistribution'.
simulateReconstructedTreeRandomHeight
  :: (PrimMonad m, NodeType c)
  => Int              -- ^ Number of points (samples)
  -> BirthRate        -- ^ Birth rate
  -> DeathRate        -- ^ Death rate
  -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
  -> m (PhyloTree Int Double c)
simulateReconstructedTreeRandomHeight n l m g = do
  t <- D.genContVar (TOD n l m) g
  simulateReconstructedTree n t l m g

-- | Use the point process to simulate a reconstructed tree (see
-- 'toReconstructedTree') with specific height and number of leaves according to
-- the birth and death process.
simulateReconstructedTree
  :: (PrimMonad m, NodeType c)
  => Int             -- ^ Number of points (samples)
  -> Time            -- ^ Time of origin
  -> BirthRate       -- ^ Birth rate
  -> DeathRate       -- ^ Death rate
  -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
  -> m (PhyloTree Int Double c)
simulateReconstructedTree n t l m g =  toReconstructedTree 0 <$> simulate n t l m g

-- | Convert a point process to a reconstructed tree. See Lemma 2.2. Of course,
-- I decided to only use one tree structure with extinct and extant leaves
-- (actually a complete tree). So a tree created here just does not contain
-- extinct leaves. A function 'isReconstructed' is provided to test if a tree is
-- reconstructed (and not complete) in this sense. However, a complete tree
-- might show up as "reconstructed", just because, by chance, it does not
-- contain extinct leaves.

-- I wanted to use a Monoid constraint to get the unit element, but this
-- fails for classical 'Int's. So, I rather have another (useless) argument.
toReconstructedTree :: (Ord a, Num b, Ord b, NodeType c)
                       => a     -- ^ Default node state.
                       -> PointProcess a b
                       -> PhyloTree a b c
toReconstructedTree defLabel pp@(PointProcess ps vs o)
  | length ps /= length vs + 1 = error "Too few or too many points."
  | length vs <= 1             = error "Too few values."
  | otherwise = if isReconstructed treeOrigin then treeOrigin else error "Error in algorithm."
  where (vsSorted, isSorted) = sort pp
        !leaves     = [ singleton p 0 | p <- ps ]
        !heights    = replicate (length ps) 0
        !treeRoot   = toReconstructedTree' defLabel isSorted vsSorted leaves heights
        !h          = last vsSorted
        !treeOrigin = lengthen (o-h) treeRoot

-- Move up the tree, connect nodes when they join according to the point process.
toReconstructedTree' :: (Num b, Ord b, NodeType c)
                     => a       -- Default node state.
                     -> [Int]   -- Sorted indices, see 'sort'.
                     -> [b]     -- Sorted merge values.
                     -> [PhyloTree a b c] -- Leaves with accumulated root branch lengths.
                     -> [b]             -- Accumulated heights of the leaves.
                     -> PhyloTree a b c
toReconstructedTree' _        [] [] trs  _ = head trs
toReconstructedTree' defLabel is vs trs hs = toReconstructedTree' defLabel is' vs' trs'' hs'
  -- For the algorithm, see 'Geneclocks.Coalescent.simulate', but index starts
  -- at zero.
  where !i     = head is
        !is'   = tail is
        !v     = head vs
        !vs'   = tail vs
        -- Left: l, right: r.
        !hl    = hs !! i
        !hr    = hs !! (i+1)
        !dvl   = v - hl
        !dvr   = v - hr
        !tl    = lengthen dvl $ trs !! i
        !tr    = lengthen dvr $ trs !! (i+1)
        !h'    = hl + dvl       -- Should be the same as 'hr + dvr'.
        !tm    = glue (PhyloLabel defLabel 0 defaultInternal) [tl, tr]
        !trs'' = take i trs ++ [tm] ++ drop (i+2) trs
        !hs'   = take i hs ++ [h'] ++ drop (i+2) hs

-- -- -- Convert a point process to a list of leaves and their branch lengths.
-- toLeaves :: (Ord b) => PointProcess a b -> [(b, PhyloTree a b)]
-- toLeaves (PointProcess ps vs _) =
--   [(v, Node (Info p v Extant) []) | (p,v) <- zip ps minVs] where
--   -- Elongate the values, so that the first and the last points also get their
--   -- share.
--   !vs'   = [head vs] ++ vs ++ [last vs]
--   -- Create a list of tuples of the neighboring values for each point.
--   !vs''  = zip vs' (tail vs')
--   -- Find the minima. These will be the branch lengths of the points.
--   !minVs = map minTuple vs''

-- -- Get the next index and value, as well as the trees that will be glued
-- -- together.
-- getHeightIndexAndTrees :: [a] -> [Int] -> [c] -> (a, Int, c, c)
-- getHeightIndexAndTrees vsS is hts = (h, i, tl, tr) where
--   !h  = head vsS
--   !i  = head is
--   !tl = hts !! i
--   !tr = hts !! (i+1)

-- -- Find the next speciation time up the tree.
-- getNextHeight :: (Ord b) => Int -> [b] -> b -> b
-- getNextHeight i vs o = minimum [hl, hr] where
--   !hl                = if i>0 then vs !! (i-1) else o
--   !hr                = if i+1<length vs then vs !! (i+1) else o

-- -- Get the heights and trees for the next call.
-- getNextHeightsAndTrees :: Int -> [a] -> a -> [b] -> [b] -> [Int] -> ([a], [b], [b], [Int])
-- getNextHeightsAndTrees i hts t vsS vs is = (hts', vsS', vs', is') where
--   !hts' = take i hts ++ [t] ++ drop (i+2) hts
--   !vsS' = tail vsS
--   !vs'  = take i vs ++ drop (i+1) vs
--   !is'  = tail is

-- -- TODO: Also improve the algorithm for the summary statistics only.

-- -- | Same as 'simulateBranchLengthNChildren' but tree height is drawn from the
-- -- expected distribution. See 'TOD.TimeOfOriginDistribution'.
-- simulateBranchLengthNChildrenRandomHeight
--   :: (PrimMonad m)
--   => Int                 -- ^ Number of points (samples)
--   -> Double              -- ^ Birth rate
--   -> Double              -- ^ Death rate
--   -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
--   -> m [(Double, Int)]
-- simulateBranchLengthNChildrenRandomHeight n l m g = do
--   t <- D.genContVar (TOD n l m) g
--   simulateBranchLengthNChildren n t l m g

-- -- | Use the point process to simulate a reconstructed tree (see
-- -- 'toReconstructedTree') with specific height and number of leaves according to
-- -- the birth and death process. For a specific branch of length 'l', this
-- -- function only returns the number of extant children.
-- simulateBranchLengthNChildren
--   :: (PrimMonad m)
--   => Int                 -- ^ Number of points (samples)
--   -> Double              -- ^ Time of origin
--   -> Double              -- ^ Birth rate
--   -> Double              -- ^ Death rate
--   -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
--   -> m [(Double, Int)]   -- ^ A list of tuples (Branch length, number of
--                          -- children).
-- simulateBranchLengthNChildren n t l m g =  toBranchLengthNChildren <$> simulate n t l m g

-- -- | See 'toReconstructedTree' and 'simulateBranchLengthNChildren'.
-- toBranchLengthNChildren :: (Num b, Ord b)
--                         => PointProcess a b
--                         -> [(b, Int)]
-- toBranchLengthNChildren pp@(PointProcess ps vs o)
--   | length ps < length vs + 1 = error "Too few points."
--   | length vs <  1            = error "Too few values."
--   | otherwise = reportLeaves ++
--                 toBranchLengthNChildren' o vs vsSorted isSorted (map leaveAddHeightAndChildren leaves)
--   where (vsSorted, isSorted) = sort pp
--         leaves = toLeaves pp
--         leaveAddChildren (l, _) = (l, 1)
--         leaveAddHeightAndChildren (l, _) = (l, l, 1)
--         reportLeaves = map leaveAddChildren leaves

-- -- See 'toBranchLengthNChildren'.
-- toBranchLengthNChildren'
--   :: (Num b, Ord b)
--   => b             -- ^ Origin (total tree height).
--   -> [b]           -- ^ The unsorted values of the point process.
--   -> [b]           -- ^ The sorted values of the point process.
--   -> [Int]         -- ^ The indices to be of the sorted values of the point process.
--   -> [(b, b, Int)] -- ^ List of total height, current branch length, and
--                    --   children of the trees that will be connected.
--   -> [(b, Int)]
-- toBranchLengthNChildren' _ _ _ _ [_]   = []
-- toBranchLengthNChildren' o vs vsS is hts = res : toBranchLengthNChildren' o vs' vsS' is' hts'
--   where
--   (!h, !i, !tl, !tr)     = getHeightIndexAndTrees vsS is hts
--   !h'                    = getNextHeight i vs o
--   !t                     = (h', h' - h, trdOfThree tl + trdOfThree tr)
--   (hts', vsS', vs', is') = getNextHeightsAndTrees i hts t vsS vs is
--   !res                   = (sndOfThree t, trdOfThree t)
