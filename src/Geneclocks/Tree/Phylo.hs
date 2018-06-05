-- Needed to derive NFData directly.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
   Module      :  Geneclocks.Tree.Phylo
   Description :  Trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Phylogenetic trees are the basis of gene, locus and species trees.

-}

module Geneclocks.Tree.Phylo
  ( module Data.Tree
  , NodeType(..)
  , Info(..)
  , PhyloTree
  , singleton
  , valid
  , totalBrLn
  , height
  , heightOfRootNode
  , glue
  , shorten
  , lengthen
  , getExtantLeaves
  , getExtinctLeaves
  , getLeaves
  , isReconstructed
  , mrcaTree
  , mrcaNode
  , mrcaTime
  , numberInternalNodes
  , heightsNSplits
  , heightsNSplitsFast
  ) where

import           Control.DeepSeq (NFData)
import           Data.List
import qualified Data.Set        as Set
import           Data.Tree
import           GHC.Generics    (Generic)

-- | Nodes should have a notion of where they are on the tree and a default state.
class (Eq n) => NodeType n where
  internal        :: n -> Bool
  extant          :: n -> Bool
  extinct         :: n -> Bool
  external        :: n -> Bool
  defaultExternal :: n
  defaultInternal :: n

  internal n = not $ extant n || extinct n
  external   = not . internal

-- | Node state of a phylogenetic tree. It contains a label of unspecified type
-- 'a', the branch length to the parent node or the origin of the tree and
-- information if the node type is internal, extinct and extant.'
data Info a b c = Info
  { label    :: a            -- ^ The label of the node, e.g., Int or String.
  , brLn     :: b            -- ^ The branch length to the parent node or the origin.
  , nodeType :: c            -- ^ Node type (e.g., 'SNode', but see "GeneIndividual").
  } deriving (Eq, Show, Generic, NFData)

instance (Ord a, Eq b, Eq c) => Ord (Info a b c) where
  compare (Info a _ _) (Info b _ _) = compare a b

-- | Phylogenetic tree data type. The node states are of type 'Info a'.
type PhyloTree a b c = Tree (Info a b c)

-- | The simplest tree. Usually an extant leaf with an attached branch.
singleton :: (NodeType c) => a -> b -> PhyloTree a b c
singleton a b = Node (Info a b defaultExternal) []

-- | Test if a tree is valid.
--
--     - extant nodes at the leaves.
--
--     - internal nodes with daughters.
valid :: (NodeType c) => PhyloTree a b c -> Bool
valid (Node (Info _ _ n) []) = extant n || extinct n
valid (Node (Info _ _ n) ts) = internal n && all valid ts

-- | Total branch length of a tree.
totalBrLn :: (Num b) => PhyloTree a b c -> b
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> b
height (Node s ts)
  | external . nodeType $ s = brLn s
  | otherwise               = maximum $ map ((+ brLn s) . height) ts

-- | Height of the tree without adding the root branch.
heightOfRootNode :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> b
-- heightOfRootNode (Node s ts) = height (Node (s {brLn = 0}) ts)
heightOfRootNode t@(Node s _) = height t - brLn s

-- | Glue branches together, so that one new tree emerges. It's root node is
-- new, the sub-forest has to be given (a list of trees).
glue :: (NodeType c)
     => Info a b c             -- ^ New root node.
     -> [PhyloTree a b c]      -- ^ Sub-forest.
     -> PhyloTree a b c
glue s@(Info _ _ n) ts
  | extant n  = error "Root node cannot be of type 'Exant'."
  | extinct n = error "Root node cannot be of type 'Extinct'."
  | otherwise = Node s ts

-- | Shorten the distance between root and origin.
shorten :: (Num b, Ord b) => b -> PhyloTree a b c -> PhyloTree a b c
shorten dl (Node (Info s l n) ts) | dl <= l   = Node (Info s (l-dl) n) ts
                                  | otherwise = error "Branch lengths cannot be negative."

-- | Lengthen the distance between root and origin.
lengthen :: (Num b, Ord b) => b -> PhyloTree a b c -> PhyloTree a b c
lengthen dl (Node (Info s l n) ts) | dl >= 0 = Node (Info s (l+dl) n) ts
                                   | otherwise = error "Branch lengths cannot be negative."

-- | Tests if a tree has any extinct leaves. If not, it is considered to be a
-- reconstructed tree structure.
isReconstructed :: (Ord a, Eq b, NodeType c)
                => PhyloTree a b c -> Bool
isReconstructed t = not $ any (extinct . nodeType) (flatten t)

-- | For a given tree, return a list of all leaves.
getExtantLeaves :: NodeType c => PhyloTree a b c -> [Info a b c]
getExtantLeaves = getLeavesWith extant

-- | For a given tree, return a list of all leaves.
getExtinctLeaves :: NodeType c => PhyloTree a b c -> [Info a b c]
getExtinctLeaves = getLeavesWith extinct

-- | For a given tree, return a list of all leaves.
getLeaves :: NodeType c => PhyloTree a b c -> [Info a b c]
getLeaves t = getExtantLeaves t ++ getExtinctLeaves t

getLeavesWith :: (c -> Bool) -> PhyloTree a b c -> [Info a b c]
getLeavesWith np (Node s ts)
  | np $ nodeType s = s : leaves
  | otherwise   = leaves
  where leaves = concatMap (getLeavesWith np) ts

getLeavesSet :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> Set.Set (Info a b c)
getLeavesSet t = Set.fromList $ getLeaves t

-- | Tree to most recent common ancestore of provided set of nodes.
mrcaTree :: (Ord a, Eq b, NodeType c)
           => Set.Set a -> PhyloTree a b c -> Maybe (PhyloTree a b c)
mrcaTree ls t
  -- XXX: This test may be slow.
  | length myLeaves /= length (Set.fromList myLeaves)
                            = error "Tree does not have unique leaf labels."
  | not $ containsMrca ls t = Nothing
  | otherwise               = Just (mrcaTreeUnsafe ls t)
  where myLeaves = getLeaves t

containsMrca :: (Ord a, Eq b, NodeType c)
       => Set.Set a -> PhyloTree a b c -> Bool
containsMrca ls t = ls `Set.isSubsetOf` Set.map label (getLeavesSet t)

-- Assumes that the mrca is on the tree.
mrcaTreeUnsafe :: (Ord a, Eq b, NodeType c)
               => Set.Set a -> PhyloTree a b c -> PhyloTree a b c
mrcaTreeUnsafe ls t = maybe t (mrcaTreeUnsafe ls) mMrcaDaughter
  where ts = subForest t
        mMrcaDaughter = find (containsMrca ls) ts

-- | Most recent common ancestor of a set of leaves.
mrcaNode :: (Ord a, Eq b, NodeType c)
     => Set.Set a -> PhyloTree a b c -> Maybe (Info a b c)
mrcaNode ls t = rootLabel <$> mrcaTree ls t

-- | Time to most recent common ancestor.
mrcaTime :: (Ord a, Ord b, Num b, NodeType c)
         => Set.Set a -> PhyloTree a b c -> Maybe b
mrcaTime ls t = heightOfRootNode <$> mrcaTree ls t

-- | Number internal nodes uniquely.
numberInternalNodes :: NodeType c => PhyloTree Int b c -> PhyloTree Int b c
numberInternalNodes t = snd $ mapAccumL myAccumulator 0 t

myAccumulator :: NodeType c => Int -> Info Int b c -> (Int, Info Int b c)
myAccumulator i n
  | internal . nodeType $ n = (i+1, n {label = i})
  | otherwise               = (i, n)

-- | Traverse a tree top down. For each split, report the corresponding subtree
-- and root node height. Note that consecutive elements can be completely
-- disjoint.
heightsNSplits :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> [(b, PhyloTree a b c)]
heightsNSplits t = (h, t) : concatMap (heightsNSplits' h) (subForest t)
  where h = heightOfRootNode t

-- | Same as 'heightsNSplits' but with known height of root node.
heightsNSplitsFast :: (Num b, Ord b, NodeType c) => b -> PhyloTree a b c -> [(b, PhyloTree a b c)]
heightsNSplitsFast h t = (h, t) : concatMap (heightsNSplits' h) (subForest t)

heightsNSplits' :: (Num b, Ord b, NodeType c)
                       => b -> PhyloTree a b c -> [(b, PhyloTree a b c)]
heightsNSplits' h t
  | external . nodeType $ lb = []
  | otherwise                = (h', t) : concatMap (heightsNSplits' h') (subForest t)
  where
    lb = rootLabel t
    dH = brLn lb
    h' = h - dH
