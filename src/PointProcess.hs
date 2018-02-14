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
import           Tree
import qualified Statistics.Distribution as D (genContVar)
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
  , tOr    :: !b } deriving (Read, Show, Eq)

-- | Sample a point process using the 'BirthDeathDistribution'. The names of the
-- points will be integers.
simulate :: (PrimMonad m)
         => Int                 -- ^ Number of points (samples)
         -> Double              -- ^ Time of origin
         -> Double              -- ^ Birth rate
         -> Double              -- ^ Death rate
         -> Gen (PrimState m)   -- ^ The generator (see 'System.Random.MWC')
         -> m (PointProcess Int Double)
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
  -> Gen (PrimState m)   -- ^ The generator (see 'System.Random.MWC')
  -> m (Tree Int Double)
simulateReconstructedTree n t l m g =  toReconstructedTree 0 <$> simulate n t l m g

-- | Convert a point process to a reconstructed tree. See Lemma 2.2. Of course,
-- I decided to only use one tree structure with extinct and extant leaves
-- (actually a complete tree). So a tree created here just does not contain
-- extinct leaves. A function 'isReconstructed' is provided to test if a tree is
-- reconstructed (and not complete) in this sense. However, a complete tree
-- might show up as "reconstructed", just because, by chance, it does not
-- contain extinct leaves.
toReconstructedTree :: (Ord b, Num b)
                    => a        -- ^ The default node state.
                    -> PointProcess a b
                    -> Tree a b
toReconstructedTree s (PointProcess ps ts t) = Origin s (t - height tr) tr
  where tr = toReconstructedTree' (map ExtantLeaf ps) ts s

-- | This is the heart of the conversion from a 'PointProcess' to a
-- 'ReconstructedTree'. The algorithm can be improved because the height of the
-- trees that are glued together is calculated over and over again. This might
-- be a time consuming operation.
toReconstructedTree' :: (Glueable t, Measurable t, Ord b, Num b)
            => [t a b]          -- ^ The 'Glueable's to glue together.
            -> [b]              -- ^ The speciation times.
            -> a                -- ^ The default node state.
            -> t a b
toReconstructedTree' [g] [] _ = g
toReconstructedTree' gs  ts s
  | length gs < length ts + 1 = error "Too few 'Glueable's."
  | length ts <  1            = error "Too few glue times."
  | otherwise                 = toReconstructedTree' (gsL ++ [g] ++ gsR) (tsL ++ tsR) s
  where
    -- Minimum time and index.
    (mT, i)      = minimum $ zip ts ([1..] :: [Int])
    -- First we need to divide the trees into the ones left and right of the
    -- pair to be glued together.
    (gsL', gsR') = splitAt i gs
    gsL          = init gsL'
    gsR          = if null gsR' then [] else tail gsR'
    -- Also the times have to be split up into left and right parts.
    (tsL', tsR) = splitAt i ts
    tsL         = init tsL'
    -- Now find the two trees that will be glued together.
    lc         = last gsL'
    rc         = head gsR'
    g          = glue s (mT - height lc) lc (mT - height rc) rc
