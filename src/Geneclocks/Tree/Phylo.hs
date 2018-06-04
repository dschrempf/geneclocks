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
  , glue
  , shorten
  , lengthen
  , getExtantLeaves
  , getExtinctLeaves
  , getLeaves
  , isReconstructed
  ) where

import           Control.DeepSeq (NFData)
import           Data.Tree
import           GHC.Generics    (Generic)

-- | Nodes should have a notion of where they are on the tree and a default state.
class NodeType n where
  internal        :: n -> Bool
  extant          :: n -> Bool
  extinct         :: n -> Bool
  defaultExternal :: n
  defaultInternal :: n

  internal n = not $ extant n || extinct n

-- | Node state of a phylogenetic tree. It contains a label of unspecified type
-- 'a', the branch length to the parent node or the origin of the tree and
-- information if the node type is internal, extinct and extant.'
data Info a b c = Info
  { label    :: a            -- ^ The label of the node, e.g., Int or String.
  , brLn     :: b            -- ^ The branch length to the parent node or the origin.
  , nodeType :: c            -- ^ Node type (e.g., PhyloNode, but see "GeneIndividual").
  } deriving (Show, Generic, NFData)

-- | Phylogenetic tree data type. The node states are of type 'Info a'.
type PhyloTree a b c = Tree (Info a b c)

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
-- valid (Node Info {}  (_:_))  = False
-- valid (Node Info {}  [])     = True

-- | Total branch length of a tree.
totalBrLn :: (Num b) => PhyloTree a b c -> b
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: (Num b, Ord b) => PhyloTree a b c -> b
height (Node s ts) = maximum $ map ((+ brLn s) . height) ts

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
isReconstructed :: (NodeType c) => PhyloTree a b c -> Bool
isReconstructed t = not $ any (extinct . nodeType) (flatten t)

-- | For a given tree, return a list of all leaves.
getExtantLeaves :: (NodeType c) => PhyloTree a b c -> [Info a b c]
getExtantLeaves = getLeavesWith extant

-- | For a given tree, return a list of all leaves.
getExtinctLeaves :: (NodeType c) => PhyloTree a b c -> [Info a b c]
getExtinctLeaves = getLeavesWith extinct

-- | For a given tree, return a list of all leaves.
getLeaves :: (NodeType c) => PhyloTree a b c -> [Info a b c]
getLeaves t = getExtantLeaves t ++ getExtinctLeaves t

getLeavesWith :: (c -> Bool) -> PhyloTree a b c -> [Info a b c]
getLeavesWith np (Node s ts)
  | np $ nodeType s = s : concatMap (getLeavesWith np) ts
  | otherwise   = concatMap (getLeavesWith np) ts
