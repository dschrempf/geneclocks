{- |
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

And then I need to write the simulator (which should be easy when the
distribution has been defined).

Finally, I have to write the bijection between a 'ReconstructedTree' and the
'PointProcess'. In the beginning, a function from any 'PointProcess' to a
'ReconstructedTree' will be enough.

-}

module PointProcess
  ( PointProcess(..)
  , simulate
  , toReconstructedTree
  , simulateReconstructedTree
  ) where

import qualified BirthDeathDistribution as D
import           Control.Monad
import           Control.Monad.Primitive
import           PhyloTree
import qualified Statistics.Distribution as D (genContVar)
import           System.Random.MWC
import           Tools

-- | A __point process__ for \(n\) points and of age \(t_{or}\) is defined as
-- follows. Draw $n$ points on the horizontal axis at \(1,2,\ldots,n\). Pick
-- \(n-1\) points at locations \((i+1/2, s_i)\), \(i=1,2,\ldots,n-1\);
-- \(0 < s_i < t_{or}\). There is a bijection between (ranked) oriented trees
-- and the point process. Usually, a will be 'String' (or 'Int') and b will be
-- 'Double'.
data PointProcess a = PointProcess
  { points :: ![a]
  , values :: ![Double]
  , origin :: !Double } deriving (Read, Show, Eq)

-- | Sample a point process using the 'BirthDeathDistribution'. The names of the
-- points will be integers.
simulate :: (PrimMonad m)
         => Int                 -- ^ Number of points (samples)
         -> Double              -- ^ Time of origin
         -> Double              -- ^ Birth rate
         -> Double              -- ^ Death rate
         -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
         -> m (PointProcess Int)
simulate n t l m g
  | n < 1     = error "Number of samples needs to be one or larger."
  | t < 0.0   = error "Time of origin needs to be positive."
  | l < 0.0   = error "Birth rate needs to be positive."
  | m < 0.0   = error "Death rate needs to be positive."
  | otherwise = do
  vs <- replicateM (n-1) (D.genContVar (D.BDD t l m) g)
  return $ PointProcess [0..(n-1)] vs t

-- | Use the point process to simulate a reconstructed tree (see
-- 'toReconstructedTree') with specific height and number of leaves according to
-- the birth and death process.
simulateReconstructedTree
  :: (PrimMonad m)
  => Int                 -- ^ Number of points (samples)
  -> Double              -- ^ Time of origin
  -> Double              -- ^ Birth rate
  -> Double              -- ^ Death rate
  -> Gen (PrimState m)   -- ^ Generator (see 'System.Random.MWC')
  -> m (PhyloTree Int)
simulateReconstructedTree n t l m g =  toReconstructedTree 0 <$> simulate n t l m g

-- | Convert a point process to a reconstructed tree. See Lemma 2.2. Of course,
-- I decided to only use one tree structure with extinct and extant leaves
-- (actually a complete tree). So a tree created here just does not contain
-- extinct leaves. A function 'isReconstructed' is provided to test if a tree is
-- reconstructed (and not complete) in this sense. However, a complete tree
-- might show up as "reconstructed", just because, by chance, it does not
-- contain extinct leaves.
toReconstructedTree :: a               -- ^ Default node state.
                    -> PointProcess a
                    -> PhyloTree a
toReconstructedTree s pp@(PointProcess ps vs o)
  | length ps < length vs + 1 = error "Too few points."
  | length vs <  1            = error "Too few values."
  | otherwise = toReconstructedTree' s o (values pp) $ toLeaves pp

-- | Internal function. Convert a point process to a list of leaves and their branch lengths.
toLeaves :: PointProcess a -> [(Double, PhyloTree a)]
toLeaves (PointProcess ps vs _) =
  [(v, Node (Info p v Extant) []) | (p,v) <- zip ps minVs] where
  -- Elongate the values, so that the first and the last points also get their
  -- share.
  vs'   = [head vs] ++ vs ++ [last vs]
  -- Create a list of tuples of the neighboring values for each point.
  vs''  = zip vs' (tail vs')
  -- Find the minima. These will be the branch lengths of the points.
  minVs = map minTuple vs''

-- | Internal function. Glue together a list of trees.
toReconstructedTree'
  :: a       -- ^ Default node state.
  -> Double  -- ^ Origin (total tree height).
  -> [Double] -- ^ The values of the point process.
  -> [(Double, PhyloTree a)] -- ^ List of trees that will be connected. The
                             -- height of the trees is also stored, so that it
                             -- does not have to be calculated repeatedly.
  -> PhyloTree a
toReconstructedTree' _ _ _  [t] = snd t
toReconstructedTree' s o vs hts = if il+1 /= ir
                                  then error "Ahh."
                                  else toReconstructedTree' s o vs hts' where
  -- Fist get the next index and value.
  (m, i) = minimumWithIndex vs
  -- First get height and index of the trees that will be connected.
  mins   = minimumsIndices (map fst hts)
  (h, i1) = head mins
  -- The heights should be the same up to machine precision.
  (_, i2) = last mins
  il     = minimum [i1, i2]
  ir     = maximum [i1, i2]
  tl     = hts !! il
  tr     = hts !! ir
  -- Now we need to find the next speciation time up the tree.
  hll    = if il>0 then fst $ hts !! (il-1) else o
  hrr    = if ir+1<length hts then fst $ hts !! (ir+1) else o
  h'     = minimum [hll, hrr]
  info   = Info s (h'-h) Internal
  t      = (h', glue info [snd tl, snd tr])
  -- The list of tree heights and trees for the next call.
  hts'    = take il hts ++ [t] ++ drop (ir+1) hts
