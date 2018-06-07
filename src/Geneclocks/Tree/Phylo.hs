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

TODO: Maybe introduce some form of partitions. Like Bipartitions.

TODO: Introduce a PhyloNode class that has all the required functionality.

-}

module Geneclocks.Tree.Phylo
  ( module Data.Tree
  , NodeType(..)
  , wrap
  , extantString
  , extinctString
  , PhyloLabel(..)
  , Info(..)
  , PhyloTree
  , rootNode
  , degree
  , rootNodesAgree
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
  , mrcaHeight
  , mrcaHeightNNode
  , mrcaHeightNTree
  , numberInternalNodes
  , heightsNSplits
  , heightsNSplitsFast
  , heightsNSplitsOrdered
  , pairwiseDistinctNodesF
  , pairwiseDistinctNodeLabels
  , pairwiseDistinctLeaveLabels
  ) where

import           Control.DeepSeq                  (NFData)
import           Data.Function                    (on)
import           Data.List
import           Data.Monoid
import qualified Data.Set                         as S
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import           Data.Tree
import           Geneclocks.Tools
import           GHC.Generics                     (Generic)

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

-- | A unified way to show information about nodes.
wrap :: Char -> String
wrap c = '[' : c : "]"

-- | Denote extant leaf.
extantString :: String
extantString = wrap '.'

-- | Denote extinct leaf.
extinctString :: String
extinctString = wrap 'X'

-- | Extract node information and branch length information from a phylogenetic tree.
class PhyloLabel l where
  nodeBuilder :: l -> B.Builder
  brLnBuilder :: l -> B.Builder

-- | Node state of a phylogenetic tree. It contains a label of unspecified type
-- 'a', the branch length to the parent node or the origin of the tree and
-- information if the node type is internal, extinct and extant.'
data Info a b c = Info
  { label    :: a            -- ^ The label of the node, e.g., Int or String.
  , brLn     :: b            -- ^ The branch length to the parent node or the origin.
  , nodeType :: c            -- ^ Node type (e.g., 'SNode', but see "GeneIndividual").
  } deriving (Eq, Show, Generic, NFData)

instance (Show a, Show c, RealFloat b) => PhyloLabel (Info a b c) where
  nodeBuilder (Info a _ c) = B.fromString (show c) <> B.fromString (show a)
  brLnBuilder (Info _ b _) = B.realFloat b

instance (Ord a, Eq b, Eq c) => Ord (Info a b c) where
  compare (Info a _ _) (Info b _ _) = compare a b

-- | Phylogenetic tree data type. The node states are of type 'Info a'.
type PhyloTree a b c = Tree (Info a b c)

-- | Get rid of the misnomer 'rootLabel', it actually should be called rootNode.
rootNode :: Tree a -> a
rootNode = rootLabel

-- | Get the root nodes of the sub forest.
daughtersRootNodes :: PhyloTree a b c -> [a]
daughtersRootNodes t = map (label . rootNode) (subForest t)

-- | Get degree of the root node. The degree is the number of daughters + 1.
degree :: PhyloTree a b c -> Int
degree t = length (subForest t) + 1

-- | For two phylogenetic trees, extract information from the root node and
-- check if it coincides. Useful to test if speciations agree, see 'iCheckHeightNSplit'.
rootNodesAgree :: (Ord a)
               => (a1 -> a) -> (a2 -> a) -> PhyloTree a1 b1 c1 -> PhyloTree a2 b2 c2 -> Bool
rootNodesAgree fs ft s t = sN == tN && S.null (S.difference sDNs tDNs)
  where sN = fs $ (label . rootNode) s
        sDNs = S.fromList $ map fs $ daughtersRootNodes s
        tN = ft $ (label . rootNode) t
        tDNs = S.fromList $ map ft $ daughtersRootNodes t

-- | The simplest tree. Usually an extant leaf with an attached branch.
singleton :: (NodeType c) => a -> b -> PhyloTree a b c
singleton a b = Node (Info a b defaultExternal) []

-- | Test if a tree is valid. Validity of a tree does not guarantee that it
-- agrees with certain constraints such as a tree of individuals necessarily
-- evolving within a tube-like species tree. See also 'iAgree' or 'gAgree'.
--
--     - extant nodes at the leaves.
--     - internal nodes with daughters.
--     - pairwise distinct node labels.
valid :: (Ord a, NodeType c) => PhyloTree a b c -> Bool
valid t = pairwiseDistinctNodeLabels t && valid' t

valid' :: (NodeType c) => PhyloTree a b c -> Bool
valid' (Node (Info _ _ n) []) = extant n || extinct n
valid' (Node (Info _ _ n) ts) = internal n && all valid' ts

-- | Total branch length of a tree.
totalBrLn :: (Num b) => PhyloTree a b c -> b
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> b
height (Node s ts)
  | external . nodeType $ s = brLn s
  | otherwise               = maximum $ map ((+ brLn s) . height) ts

-- TODO: Height function for clock-like trees. Much faster, because one only
-- needs to traverse tree until first extant node.

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

-- getLeavesSet :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> S.Set (Info a b c)
-- getLeavesSet t = S.fromList $ getLeaves t

-- | Extract some value that needs to be unique for each node on the given tree.
pairwiseDistinctNodesF :: Ord d => (Info a b c -> d) -> PhyloTree a b c -> Bool
pairwiseDistinctNodesF f t = length myNodeInfos == length (S.fromList myNodeInfos)
  where myNodes     = flatten t
        myNodeInfos = map f myNodes

-- | Check if a tree has unique node labels.
pairwiseDistinctNodeLabels :: (Ord a) => PhyloTree a b c -> Bool
pairwiseDistinctNodeLabels = pairwiseDistinctNodesF label

-- | Check if a tree has unique leaf labels.
pairwiseDistinctLeaveLabels :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> Bool
pairwiseDistinctLeaveLabels t = isPairwiseDistinct myLeaves
  where myLeaves = getLeaves t

-- | Tree to most recent common ancestore of provided set of nodes.
mrcaTree :: (Ord a, Eq b, NodeType c)
           => S.Set a -> PhyloTree a b c -> Maybe (PhyloTree a b c)
mrcaTree ls t
  -- XXX: This test may be slow.
  | not $ pairwiseDistinctLeaveLabels t = error "Tree does not have pairwise distinct leaf labels."
  | not $ containsMrca ls t   = Nothing
  | otherwise                 = Just (mrcaTreeUnsafe ls t)

containsMrca :: (Ord a, Eq b, NodeType c)
       => S.Set a -> PhyloTree a b c -> Bool
containsMrca ls t = ls `S.isSubsetOf` S.fromList (map label (flatten t))

-- Assumes that the mrca is on the tree.
mrcaTreeUnsafe :: (Ord a, Eq b, NodeType c)
               => S.Set a -> PhyloTree a b c -> PhyloTree a b c
mrcaTreeUnsafe ls t = maybe t (mrcaTreeUnsafe ls) mMrcaDaughter
  where ts = subForest t
        mMrcaDaughter = find (containsMrca ls) ts

-- | Most recent common ancestor of a set of leaves.
mrcaNode :: (Ord a, Eq b, NodeType c)
     => S.Set a -> PhyloTree a b c -> Maybe (Info a b c)
mrcaNode ls t = rootNode <$> mrcaTree ls t

-- | Height of most recent common ancestor.
mrcaHeight :: (Ord a, Ord b, Num b, NodeType c)
         => S.Set a -> PhyloTree a b c -> Maybe b
mrcaHeight ls t = heightOfRootNode <$> mrcaTree ls t

-- | Height and node of MRCA.
mrcaHeightNTree :: (Ord a, Ord b, Num b, NodeType c)
                => S.Set a -> PhyloTree a b c -> Maybe (b, PhyloTree a b c)
mrcaHeightNTree ls tr = maybe Nothing (\t -> Just (heightOfRootNode t, t)) mT
  where mT = mrcaTree ls tr

-- | Height and node of MRCA.
mrcaHeightNNode :: (Ord a, Ord b, Num b, NodeType c)
                => S.Set a -> PhyloTree a b c -> Maybe (b, Info a b c)
mrcaHeightNNode ls tr = maybe Nothing (\t -> Just (heightOfRootNode t, rootNode t)) mT
  where mT = mrcaTree ls tr

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
    lb = rootNode t
    dH = brLn lb
    h' = h - dH

-- | Same as 'heightsNSPlits' but ordered with respect to height, ascending.
heightsNSplitsOrdered :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> [(b, PhyloTree a b c)]
heightsNSplitsOrdered t = sortBy (compare `on` fst) $ heightsNSplits t

-- -- | Test if root node type agrees with a predicate.
-- rootNodeTypeP :: (c -> Bool) -> PhyloTree a b c -> Bool
-- rootNodeTypeP p t = p $ nodeType (rootLabel t)

-- -- | From a list of trees, only keep trees with root nodes passing a predicate.
-- filterRootNodes :: (c -> Bool) -> [PhyloTree a b c] -> [PhyloTree a b c]
-- filterRootNodes p ts = filter (rootNodeTypeP p) ts
