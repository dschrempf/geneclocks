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
  , PhyloNode(..)
  , Info(..)
  , PhyloTree
  , singleton
  , valid
  , totalBrLn
  , height
  , glue
  , shorten
  , lengthen
  , getExtantLeaves
  , getExtinctLeaves
  , getLeaves
  , isReconstructed
  ) where

import           Control.DeepSeq
import           Data.Tree
import           GHC.Generics                     (Generic)

-- | Node type of a phylogenetic tree. Technically, the type 'Internal' is not
-- necessary because it can be deduced from the tree. However, it is convenient
-- to save the type in this way.
data PhyloNode = Internal            -- ^ Internal node.
               | Extant              -- ^ Extant leaf.
               | Extinct             -- ^ Extinct leaf.
               deriving (Eq, Read, Show, Generic, NFData)

-- | Node state of a phylogenetic tree. It contains a label of unspecified type
-- 'a', the branch length to the parent node or the origin of the tree and
-- information if the node type is internal, extinct and extant.'
data Info a b = Info
  { label :: a            -- ^ The label of the node, e.g., Int or String.
  , brLn  :: b            -- ^ The branch length to the parent node or the origin.
  , node  :: PhyloNode } deriving (Show, Generic, NFData)

-- | Phylogenetic tree data type. The node states are of type 'Info a'.
type PhyloTree a b = Tree (Info a b)

singleton :: a -> b -> PhyloTree a b
singleton a b = Node (Info a b Extant) []

-- | Test if a tree is valid.
--
--   * no extant or extinct leaves at internal nodes
--
--   * no internal nodes at the leaves.
valid :: PhyloTree a b -> Bool
valid (Node (Info _ _ Internal) []) = False
valid (Node (Info _ _ Internal) ts) = all valid ts
valid (Node Info {}  (_:_))         = False
valid (Node Info {}  [])            = True

-- | Total branch length of a tree.
totalBrLn :: (Num b) => PhyloTree a b -> b
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: (Num b, Ord b) => PhyloTree a b -> b
height (Node s ts) = maximum $ map ((+ brLn s) . height) ts

-- | Glue branches together, so that one new tree emerges. It's root node is
-- new, the sub-forest has to be given (a list of trees).
glue :: Info a b               -- ^ New root node.
     -> [PhyloTree a b]        -- ^ Sub-forest.
     -> PhyloTree a b
glue (Info _ _ Extant)  _ = error "Root node cannot be of type 'Exant'."
glue (Info _ _ Extinct) _ = error "Root node cannot be of type 'Extinct'."
glue s ts                 = Node s ts

-- | Shorten the distance between root and origin.
shorten :: (Num b, Ord b) => b -> PhyloTree a b -> PhyloTree a b
shorten dl (Node (Info s l n) ts) | dl <= l   = Node (Info s (l-dl) n) ts
                                  | otherwise = error "Branch lengths cannot be negative."

-- | Lengthen the distance between root and origin.
lengthen :: (Num b, Ord b) => b -> PhyloTree a b -> PhyloTree a b
lengthen dl (Node (Info s l n) ts) | dl >= 0 = Node (Info s (l+dl) n) ts
                                   | otherwise = error "Branch lengths cannot be negative."

-- | Tests if a tree has extinct leaves. If not, it is considered to be a
-- reconstructed tree structure.
isReconstructed :: PhyloTree a b -> Bool
isReconstructed t = notElem Extinct $ map node (flatten t)

-- | For a given tree, return a list of all leaves.
getExtantLeaves :: PhyloTree a b -> [Info a b]
getExtantLeaves = getLeavesWith Extant

-- | For a given tree, return a list of all leaves.
getExtinctLeaves :: PhyloTree a b -> [Info a b]
getExtinctLeaves = getLeavesWith Extinct

-- | For a given tree, return a list of all leaves.
getLeaves :: PhyloTree a b -> [Info a b]
getLeaves t = getLeavesWith Extinct t ++ getLeavesWith Extant t

getLeavesWith :: PhyloNode -> PhyloTree a b -> [Info a b]
getLeavesWith n (Node s ts)
  | node s == n = s : concatMap getLeaves ts
  | otherwise   = concatMap getLeaves ts
