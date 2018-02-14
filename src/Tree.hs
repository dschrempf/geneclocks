{- |
   Description :  Trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Basis definitions for trees

-}

module Tree
  ( Glueable(..)
  , Measurable(..)
  , Tree(..)
  , toNewick
  , toNewickString
  , toNewickInt
  , toNewickWith
  , labelLeaves
  , getExtantLeaves
  , getExtinctLeaves
  , getLeaves
  , isReconstructed
  ) where

import qualified Data.Text as T
import qualified Tools

-- | The strict binary tree data type with node states of type a. The branch
-- lengths of type b is the length from the current to the left and right child.
-- See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
-- Theoretical Biology, 253(4), 769–778.
-- http://doi.org/10.1016/j.jtbi.2008.04.005
data Tree a b = Origin { state :: !a
                               , brLn  :: !b
                               , chld  :: Tree a b }
                      | Node { state :: !a
                             , lBrLn :: !b
                             , lChld :: Tree a b
                             , rBrLn :: !b
                             , rChld :: Tree a b }
                      | ExtantLeaf { state :: !a }
                      | ExtinctLeaf { state :: !a }
                      deriving (Eq, Show, Read)

-- | Make (Tree a) a functor. Like this, we can scale branch lengths or convert
-- them to transition probability matrices.
instance Functor (Tree a) where
  fmap f (Origin s b c) = Origin s (f b) (fmap f c)
  fmap f (Node s lb lc rb rc) = Node s (f lb) (fmap f lc) (f rb) (fmap f rc)
  fmap _ (ExtantLeaf s) = ExtantLeaf s
  fmap _ (ExtinctLeaf s) = ExtinctLeaf s

-- | Trees can be glued together. Take two trees (the left and right children)
-- and glue them onto a new node.
class Glueable t where
  -- | Glue two branches together, so that a new tree emerges. It's root node is
  -- new and the left and right branches are the given branches.
  glue :: (Num b)
       => a                     -- ^ The new node.
       -> b                     -- ^ The left branch.
       -> t a b                 -- ^ And the left child tree.
       -> b                     -- ^ The right branch.
       -> t a b                 -- ^ And the right child tree.
       -> t a b
  -- | Connect a tree with N extant leafs to N trees, so that a new, larger tree
  -- emerges.
  connect :: (Num b)
          => t a b              -- ^ The parent tree (the one at the top).
          -> [t a b]              -- ^ The children. The length has to equal the
                                  -- number of extant leafs of the parent.
          -> t a b

class Measurable t where
  totalBrLn :: (Num b) => t a b -> b
  height :: (Num b, Ord b) => t a b -> b

-- | Take care when gluing trees with origins because their state will be pruned
-- and lost.
instance Glueable Tree where
  glue s lb lc rb rc = Node s (lb + ll) lcc (rb + rl) rcc
    where (lcc, ll)  = removeOrigin lc
          (rcc, rl)  = removeOrigin rc
  connect p cs       = if length cs /= length (getExtantLeaves p)
                          then error "Number of leaves and children mismatch."
                          else fst $ connect' p cs 0

-- | See also 'labelLeavesWith''.
connect' :: (Num b)
         => Tree a b
         -> [Tree a b]
         -> Int                 -- ^ This is now the leaf number.
         -> (Tree a b, Int)     -- ^ The leave number needs to be tracked.
connect' (Origin s b c)  cs n = (Origin s b c', n')
  where (c', n') = connect' c cs n
connect' (ExtinctLeaf s) _  n = (ExtinctLeaf s, n)
connect' (ExtantLeaf  s) cs n = undefined

-- | Remove a possible origin and also return the branch length between root and
-- origin.
removeOrigin :: Num b => Tree a b -> (Tree a b, b)
removeOrigin (Origin _ l c) = (c, l)
removeOrigin t = (t, 0)

instance Measurable Tree where
  -- | The total branch length; only works when the branch lengths are numbers.
  totalBrLn (Origin _ b c)       = b + totalBrLn c
  totalBrLn (Node _ lB lC rB rC) = lB + rB + totalBrLn lC + totalBrLn rC
  totalBrLn (ExtantLeaf _)       = 0
  totalBrLn (ExtinctLeaf _)      = 0

  height (Origin _ b c) = b + height c
  height (Node _ lB lC rB rC) = max (lB + height lC) (rB + height rC)
  height (ExtantLeaf _) = 0
  height (ExtinctLeaf _) = 0

-- | For a given tree, return a list of extant leaves.
getExtantLeaves :: Tree a b -> [a]
getExtantLeaves (Origin _ _ c) = getExtantLeaves c
getExtantLeaves (Node _ _ lc _ rc) = getExtantLeaves lc ++ getExtantLeaves rc
getExtantLeaves (ExtantLeaf s) = [s]
getExtantLeaves (ExtinctLeaf _) = []

-- | For a given tree, return a list of extinct leaves.
getExtinctLeaves :: Tree a b -> [a]
getExtinctLeaves (Origin _ _ c) = getExtinctLeaves c
getExtinctLeaves (Node _ _ lc _ rc) = getExtinctLeaves lc ++ getExtinctLeaves rc
getExtinctLeaves (ExtantLeaf _) = []
getExtinctLeaves (ExtinctLeaf s) = [s]

-- | For a given tree, return a list of all leaves.
getLeaves :: Tree a b -> [a]
getLeaves (Origin _ _ c) = getLeaves c
getLeaves (Node _ _ lc _ rc) = getLeaves lc ++ getLeaves rc
getLeaves (ExtantLeaf s) = [s]
getLeaves (ExtinctLeaf s) = [s]

-- | Convert a tree with text nodes and branch lengths into a Newick string.
toNewick :: (RealFloat b) => Tree T.Text b -> T.Text
toNewick = toNewickWith id Tools.realFloatToText

-- | Convert a tree with text nodes and branch lengths into a Newick string.
toNewickString :: (RealFloat b) => Tree String b -> T.Text
toNewickString = toNewickWith T.pack Tools.realFloatToText

-- | Convert a tree with text nodes and branch lengths into a Newick string.
toNewickInt :: (Integral a, RealFloat b) => Tree a b -> T.Text
toNewickInt = toNewickWith Tools.intToText Tools.realFloatToText

-- | General conversion of a tree into a Newick string. Use provided functions
-- to convert node states and branches to text objects.
toNewickWith :: (a -> T.Text) -> (b -> T.Text) -> Tree a b -> T.Text
toNewickWith fn fb t = toNewick' t `T.append` T.pack ";"
  where
    toNewick' (ExtantLeaf s)  = fn s
    toNewick' (ExtinctLeaf s) = fn s
    toNewick' (Origin s b c)  =
      T.concat [ T.pack "(" , toNewick' c , T.pack ":", fb b, T.pack ")", fn s ]
    toNewick' (Node s lb lc rb rc) =
      T.concat [ T.pack "("
               , toNewick' lc, T.pack ":", fb lb, T.pack ","
               , toNewick' rc, T.pack ":", fb rb, T.pack ")",  fn s ]

-- | Label the leaves of a tree. The labels will be computed according to a
-- function 'f :: Int -> a'. This will OVERWRITE the leaf labels.
labelLeaves :: Tree T.Text b -> Tree T.Text b
labelLeaves = labelLeavesWith Tools.intToText

labelLeavesWith :: (Int -> a) -> Tree a b -> Tree a b
labelLeavesWith f t = fst $ labelLeavesWith' f t 0

labelLeavesWith' :: (Int -> a) -> Tree a b -> Int -> (Tree a b, Int)
labelLeavesWith' f (Origin s b c) n  = (Origin s b c', n')
  where (c', n') = labelLeavesWith' f c n
labelLeavesWith' f (ExtantLeaf _) n  = (ExtantLeaf (f n), n+1)
labelLeavesWith' f (ExtinctLeaf _) n = (ExtinctLeaf (f n), n+1)
labelLeavesWith' f (Node s lB lC rB rC) n = (Node s lB lC' rB rC', n'')
  where (lC', n' ) = labelLeavesWith' f lC n
        (rC', n'') = labelLeavesWith' f rC n'

-- | Tests if a tree has extinct leaves. If not, it is considered to be a
-- reconstructed tree structure.
isReconstructed :: Tree a b -> Bool
isReconstructed (ExtinctLeaf _) = False
isReconstructed (ExtantLeaf  _) = True
isReconstructed (Origin _ _  c) = isReconstructed c
isReconstructed (Node _ _ lc _ rc) = isReconstructed lc && isReconstructed rc
