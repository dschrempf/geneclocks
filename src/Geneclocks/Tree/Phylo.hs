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
  , BuilderLabel(..)
  , PhyloLabel(..)
  , PhyloTree
  , rootNodeState
  , rootNodeBrLn
  , rootNodeType
  , rootDegree
  , rootNodesAgree
  , singleton
  , valid
  , totalBrLn
  , height
  , clockLike
  , distancesRootExtantLeaves
  , heightClockLike
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
  , numberNodes
  , heightsNSplits
  , heightsNSplitsFast
  , heightsNSplitsOrdered
  , pairwiseDistinctNodesF
  , pairwiseDistinctNodeStates
  , pairwiseDistinctLeaveLabels
  ) where

import           Control.DeepSeq                  (NFData)
import           Data.Foldable
import           Data.Function                    (on)
import           Data.List
import           Data.Monoid
import qualified Data.Set                         as S
import qualified Data.Text.Lazy.Builder           as B
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

-- | Node state of a phylogenetic tree. It contains a label of unspecified type
-- 'a', the branch length to the parent node or the origin of the tree and
-- information if the node type is internal, extinct and extant.'
data PhyloLabel a b c = PhyloLabel
  { state     :: a            -- ^ The label of the node, e.g., Int or String.
  , brLn     :: b            -- ^ The branch length to the parent node or the origin.
  , nodeType :: c            -- ^ Node type (e.g., 'SNode', but see "GeneIndividual").
  } deriving (Eq, Show, Generic, NFData)

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
class BuilderLabel l where
  nodeBuilder :: l -> B.Builder
  brLnBuilder :: l -> B.Builder

instance (Show a, Show c, RealFloat b) => BuilderLabel (PhyloLabel a b c) where
  nodeBuilder (PhyloLabel a _ c) = B.fromString (show c) <> B.fromString (show a)
  brLnBuilder (PhyloLabel _ b _) = realFloatBuilder b

instance (Ord a, Eq b, Eq c) => Ord (PhyloLabel a b c) where
  compare (PhyloLabel a _ _) (PhyloLabel b _ _) = compare a b

-- | Phylogenetic tree data type. The node states are of type 'PhyloLabel a'.
type PhyloTree a b c = Tree (PhyloLabel a b c)

-- | Get state of root node.
rootNodeState :: PhyloTree a b c -> a
rootNodeState = state . rootLabel

-- | Get length of branch connected to the root node.
rootNodeBrLn :: PhyloTree a b c -> b
rootNodeBrLn = brLn . rootLabel

-- | Get node type of root node.
rootNodeType :: PhyloTree a b c -> c
rootNodeType = nodeType . rootLabel

-- | Get the root nodes of the sub forest.
rootNodeStatesOfDaughters :: PhyloTree a b c -> [a]
rootNodeStatesOfDaughters t = map rootNodeState (subForest t)

-- | Get degree of the root node. The degree is the number of daughters + 1.
rootDegree :: PhyloTree a b c -> Int
rootDegree t = length (subForest t) + 1

-- | For two phylogenetic trees, extract information from the root node and
-- check if it coincides. Useful to test if speciations agree, see 'iCheckHeightNSplit'.
rootNodesAgree :: (Ord a)
               => (a1 -> a) -> (a2 -> a) -> PhyloTree a1 b1 c1 -> PhyloTree a2 b2 c2 -> Bool
rootNodesAgree fs ft s t = sN == tN && S.null (S.difference sDNs tDNs)
  where sN = fs $ rootNodeState s
        sDNs = S.fromList $ map fs $ rootNodeStatesOfDaughters s
        tN = ft $ rootNodeState t
        tDNs = S.fromList $ map ft $ rootNodeStatesOfDaughters t

-- | The simplest tree. Usually an extant leaf with an attached branch.
singleton :: (NodeType c) => a -> b -> PhyloTree a b c
singleton a b = Node (PhyloLabel a b defaultExternal) []

-- | Test if a tree is valid. Validity of a tree does not guarantee that it
-- agrees with certain constraints such as a tree of individuals necessarily
-- evolving within a tube-like species tree. See also 'iAgree' or 'gAgree'.
--
--     - extant nodes at the leaves.
--     - internal nodes with daughters.
--     - pairwise distinct node labels.
valid :: (Ord a, NodeType c) => PhyloTree a b c -> Bool
valid t = pairwiseDistinctNodeStates t && valid' t

valid' :: (NodeType c) => PhyloTree a b c -> Bool
valid' (Node (PhyloLabel _ _ n) []) = extant n || extinct n
valid' (Node (PhyloLabel _ _ n) ts) = internal n && all valid' ts

-- | Total branch length of a tree.
totalBrLn :: (Num b) => PhyloTree a b c -> b
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> b
height t
  | external . rootNodeType $ t = rootNodeBrLn t
  | otherwise               = maximum $ map ((+ rootNodeBrLn t) . height) (subForest t)

-- | Check if a tree is clock-like, i.e., when the distances between the root
-- and extant leaves are equal .
clockLike :: (Eq b, Num b, NodeType c) => PhyloTree a b c -> Bool
clockLike t = all (== head ds) (tail ds)
  where ds = distancesRootExtantLeaves t

-- | Get the distances from the root to all extant leaves.
distancesRootExtantLeaves :: (Num b, NodeType c) => PhyloTree a b c -> [b]
distancesRootExtantLeaves t
  | extant . rootNodeType $ t = [bl]
  | otherwise                 = concatMap (map (+ bl) . distancesRootExtantLeaves) (subForest t)
    where bl = rootNodeBrLn t

-- | Get height of clock-like tree, i.e., when the distances between the root and
-- extant leaves are equal . This function returns the distance between the root
-- and the first extant leaf it finds, so the behavior is undefined when the
-- tree is NOT clock-like.
heightClockLike :: (Num b, NodeType c) => PhyloTree a b c -> Maybe b
heightClockLike t
  | extant . rootNodeType $ t  = Just bl
  | extinct . rootNodeType $ t = Nothing
  -- 'asum' takes the first Just value.
  | otherwise                  = asum $ map (fmap (+ bl) . heightClockLike) (subForest t)
  where bl = rootNodeBrLn t

-- | Height of the tree without adding the root branch.
heightOfRootNode :: (Num b, Ord b, NodeType c) => PhyloTree a b c -> b
-- heightOfRootNode (Node s ts) = height (Node (s {brLn = 0}) ts)
heightOfRootNode t@(Node s _) = height t - brLn s

-- | Glue branches together, so that one new tree emerges. It's root node is
-- new, the sub-forest has to be given (a list of trees).
glue :: (NodeType c)
     => PhyloLabel a b c             -- ^ New root node.
     -> [PhyloTree a b c]      -- ^ Sub-forest.
     -> PhyloTree a b c
glue s@(PhyloLabel _ _ n) ts
  | extant n  = error "Root node cannot be of type 'Exant'."
  | extinct n = error "Root node cannot be of type 'Extinct'."
  | otherwise = Node s ts

-- | Shorten the distance between root and origin.
shorten :: (Num b, Ord b) => b -> PhyloTree a b c -> PhyloTree a b c
shorten dl (Node (PhyloLabel s l n) ts) | dl <= l   = Node (PhyloLabel s (l-dl) n) ts
                                  | otherwise = error "Branch lengths cannot be negative."

-- | Lengthen the distance between root and origin.
lengthen :: (Num b, Ord b) => b -> PhyloTree a b c -> PhyloTree a b c
lengthen dl (Node (PhyloLabel s l n) ts) | dl >= 0 = Node (PhyloLabel s (l+dl) n) ts
                                   | otherwise = error "Branch lengths cannot be negative."

-- | Tests if a tree has any extinct leaves. If not, it is considered to be a
-- reconstructed tree structure.
isReconstructed :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> Bool
isReconstructed t = not $ any (extinct . nodeType) (flatten t)

-- | For a given tree, return a list of all leaves.
getExtantLeaves :: NodeType c => PhyloTree a b c -> [PhyloLabel a b c]
getExtantLeaves = getLeavesWith extant

-- | For a given tree, return a list of all leaves.
getExtinctLeaves :: NodeType c => PhyloTree a b c -> [PhyloLabel a b c]
getExtinctLeaves = getLeavesWith extinct

-- | For a given tree, return a list of all leaves.
getLeaves :: NodeType c => PhyloTree a b c -> [PhyloLabel a b c]
getLeaves t = getExtantLeaves t ++ getExtinctLeaves t

getLeavesWith :: (c -> Bool) -> PhyloTree a b c -> [PhyloLabel a b c]
getLeavesWith np (Node s ts)
  | np $ nodeType s = s : leaves
  | otherwise   = leaves
  where leaves = concatMap (getLeavesWith np) ts

-- getLeavesSet :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> S.Set (PhyloLabel a b c)
-- getLeavesSet t = S.fromList $ getLeaves t

-- | Extract some value that needs to be unique for each node on the given tree.
pairwiseDistinctNodesF :: Ord d => (PhyloLabel a b c -> d) -> PhyloTree a b c -> Bool
pairwiseDistinctNodesF f t = length myNodeLabels == length (S.fromList myNodeLabels)
  where myNodes     = flatten t
        myNodeLabels = map f myNodes

-- | Check if a tree has unique node labels.
pairwiseDistinctNodeStates :: (Ord a) => PhyloTree a b c -> Bool
pairwiseDistinctNodeStates = pairwiseDistinctNodesF state

-- | Check if a tree has unique leaf labels.
pairwiseDistinctLeaveLabels :: (Ord a, Eq b, NodeType c) => PhyloTree a b c -> Bool
pairwiseDistinctLeaveLabels = isPairwiseDistinct . getLeaves

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
containsMrca ls t = ls `S.isSubsetOf` S.fromList (map state (flatten t))

-- Assumes that the mrca is on the tree.
mrcaTreeUnsafe :: (Ord a, Eq b, NodeType c)
               => S.Set a -> PhyloTree a b c -> PhyloTree a b c
mrcaTreeUnsafe ls t = maybe t (mrcaTreeUnsafe ls) mMrcaDaughter
  where ts = subForest t
        mMrcaDaughter = find (containsMrca ls) ts

-- | Most recent common ancestor of a set of leaves.
mrcaNode :: (Ord a, Eq b, NodeType c)
     => S.Set a -> PhyloTree a b c -> Maybe (PhyloLabel a b c)
mrcaNode ls t = rootLabel <$> mrcaTree ls t

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
                => S.Set a -> PhyloTree a b c -> Maybe (b, PhyloLabel a b c)
mrcaHeightNNode ls tr = maybe Nothing (\t -> Just (heightOfRootNode t, rootLabel t)) mT
  where mT = mrcaTree ls tr

-- | Number internal nodes pairwise distinctly.
numberInternalNodes :: NodeType c => PhyloTree Int b c -> PhyloTree Int b c
numberInternalNodes t = snd $ mapAccumL myAccumulatorInternalOnly 0 t

myAccumulatorInternalOnly :: NodeType c => Int -> PhyloLabel Int b c -> (Int, PhyloLabel Int b c)
myAccumulatorInternalOnly i n
  | internal . nodeType $ n = (i+1, n {state = i})
  | otherwise               = (i, n)

-- | Number all nodes pairwise distinctly.
numberNodes :: NodeType c => PhyloTree Int b c -> PhyloTree Int b c
numberNodes t = snd $ mapAccumL myAccumulator 0 t

myAccumulator :: NodeType c => Int -> PhyloLabel Int b c -> (Int, PhyloLabel Int b c)
myAccumulator i n = (i+1, n {state = i})

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
  | external . rootNodeType $ t = []
  | otherwise                = (h', t) : concatMap (heightsNSplits' h') (subForest t)
  where
    dH = rootNodeBrLn t
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
