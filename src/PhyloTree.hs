-- Needed to derive NFData directly.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
   Description :  Trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Basis definitions for trees

-}

module PhyloTree
  ( module Data.Tree
  , PhyloNode(..)
  , Info(..)
  , PhyloTree
  , valid
  , totalBrLn
  , height
  , glue
  , shorten
  , toNewick
  , toNewickString
  , toNewickInt
  , toNewickWith
  , toNewickWithBuilder
  , getExtantLeaves
  , getExtinctLeaves
  , getLeaves
  , isReconstructed
  , BrLnNChildren
  , NChildSumStat
  , formatNChildSumStat
  ) where

import           Control.DeepSeq
import           Data.List                        (intersperse)
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as T (toStrict)
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import           Data.Tree
import           GHC.Generics                     (Generic)
import qualified Tools

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
data Info a = Info
  { label :: a      -- ^ The label of the node, e.g., Int or String.
  , brLn  :: Double -- ^ The branch length to the parent node or the origin.
  , node  :: PhyloNode } deriving (Show, Generic, NFData)

-- | Phylogenetic tree data type. The node states are of type 'Info a'.
type PhyloTree a = Tree (Info a)

-- | Test if a tree is valid.
--
--   * no extant or extinct leaves at internal nodes
--
--   * no internal nodes at the leaves.
valid :: PhyloTree a -> Bool
valid (Node (Info _ _ Internal) []) = False
valid (Node (Info _ _ Internal) ts) = all valid ts
valid (Node Info {}  (_:_))         = False
valid (Node Info {}  [])            = True

-- | Total branch length of a tree.
totalBrLn :: PhyloTree a -> Double
totalBrLn (Node s ts) = brLn s + sum (map totalBrLn ts)

-- | The height of the tree (the maximum branch length from root to leaves).
height :: PhyloTree a -> Double
height (Node s ts) = maximum $ map ((+ brLn s) . height) ts

-- | Glue branches together, so that one new tree emerges. It's root node is
-- new, the sub-forest has to be given (a list of trees).
glue :: Info a                -- ^ New root node.
     -> [PhyloTree a]         -- ^ Sub-forest.
     -> PhyloTree a
glue (Info _ _ Extant)  _ = error "Root node cannot be of type 'Exant'."
glue (Info _ _ Extinct) _ = error "Root node cannot be of type 'Extinct'."
glue s ts                 = Node s ts

-- | Shorten the distance between root and origin.
shorten :: PhyloTree a -> Double -> PhyloTree a
shorten (Node (Info s l n) ts) l' = Node (Info s (l-l') n) ts

-- -- | Connect a tree with N extant leaves to N trees, so that a new, larger tree
-- -- emerges.
-- connect :: Tree a              -- ^ Parent tree.
--         -> [t a]            -- ^ Children that will be attached to the leaves.
--         -> t a
-- connect p cs = if length cs /= length (getExtantLeaves p)
--                then error "Number of leaves and children mismatch."
--                else fst $ connect' p cs 0

-- -- | See also 'labelLeavesWith''.
-- connect' :: (Num b)
--          => Tree a b
--          -> [Tree a b]
--          -> Int                 -- ^ This is now the leaf number.
--          -> (Tree a b, Int)     -- ^ The leave number needs to be tracked.
-- connect' (Origin s b c)  cs n = (Origin s b c', n')
--   where (c', n') = connect' c cs n
-- connect' (ExtinctLeaf s) _  n = (ExtinctLeaf s, n)
-- connect' (ExtantLeaf  s) cs n = undefined

-- | For a given tree, return a list of all leaves.
getExtantLeaves :: PhyloTree a -> [Info a]
getExtantLeaves = getLeavesWith Extant

-- | For a given tree, return a list of all leaves.
getExtinctLeaves :: PhyloTree a -> [Info a]
getExtinctLeaves = getLeavesWith Extinct

-- | For a given tree, return a list of all leaves.
getLeaves :: PhyloTree a -> [Info a]
getLeaves t = getLeavesWith Extinct t ++ getLeavesWith Extant t

getLeavesWith :: PhyloNode -> PhyloTree a -> [Info a]
getLeavesWith n (Node s ts)
  | node s == n = s : concatMap getLeaves ts
  | otherwise   = concatMap getLeaves ts

-- | Convert a phylogenetic tree with text node labels into a Newick text object.
toNewick :: PhyloTree T.Text -> T.Text
toNewick = toNewickWith id

-- | Convert a phylogenetic tree with string node labels into a Newick text object.
toNewickString :: PhyloTree String -> T.Text
toNewickString = toNewickWith T.pack

-- | Convert a phylogenetic tree with integral node labels into a Newick text
-- object. This function is preferable because it uses the text builder and is
-- much faster.
toNewickInt :: Integral a => PhyloTree a -> T.Text
toNewickInt t = T.toStrict $ B.toLazyText $ toNewickWithBuilder B.decimal t

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided functions to convert node states and branches to text objects.
-- See also Biobase.Newick.Export.
toNewickWith :: (a -> T.Text) -> PhyloTree a -> T.Text
toNewickWith f t = go t `T.append` T.pack ";"
  where
    go (Node s [])   = lbl s
    go (Node s ts)   = T.pack "(" `T.append`
                         T.concat (intersperse (T.pack ",") $ map go ts)
                         `T.append` T.pack ")" `T.append` lbl s
    lbl (Info s l _) = f s `T.append`
                       T.pack ":" `T.append` Tools.realFloatToText l

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided text builders to convert node states and branches to text
-- objects.
toNewickWithBuilder :: (a -> B.Builder) -> PhyloTree a -> B.Builder
toNewickWithBuilder f t = go t `mappend` B.singleton ';'
  where
    go (Node s [])   = lbl s
    go (Node s ts)   = B.singleton '(' `mappend`
                         mconcat (intersperse (B.singleton ',') $ map go ts)
                         `mappend` B.singleton ')' `mappend` lbl s
    lbl (Info s l _) = f s `mappend` B.singleton ':' `mappend` B.realFloat l

-- -- | Label the leaves of a tree. The labels will be computed according to a
-- -- function 'f :: Int -> a'. This will OVERWRITE the leaf labels.
-- labelLeaves :: Tree T.Text b -> Tree T.Text b
-- labelLeaves = labelLeavesWith Tools.intToText

-- labelLeavesWith :: (Int -> a) -> Tree a b -> Tree a b
-- labelLeavesWith f t = fst $ labelLeavesWith' f t 0

-- labelLeavesWith' :: (Int -> a) -> Tree a b -> Int -> (Tree a b, Int)
-- labelLeavesWith' f (Origin s b c) n  = (Origin s b c', n')
--   where (c', n') = labelLeavesWith' f c n
-- labelLeavesWith' f (ExtantLeaf _) n  = (ExtantLeaf (f n), n+1)
-- labelLeavesWith' f (ExtinctLeaf _) n = (ExtinctLeaf (f n), n+1)
-- labelLeavesWith' f (Node s lB lC rB rC) n = (Node s lB lC' rB rC', n'')
--   where (lC', n' ) = labelLeavesWith' f lC n
--         (rC', n'') = labelLeavesWith' f rC n'

-- | Tests if a tree has extinct leaves. If not, it is considered to be a
-- reconstructed tree structure.
isReconstructed :: PhyloTree a -> Bool
isReconstructed t = notElem Extinct $ map node (flatten t)

-- | Pair of branch length with number of extant children.
type BrLnNChildren = (Double, Int)

-- | Possible summary statistic of phylogenetic trees. A list of tuples
-- (BranchLength, NumberOfExtantChildrenBelowThisBranch).
type NChildSumStat = [BrLnNChildren]

-- | Format the summary statistics in the following form:
-- @
--    nLeaves1 branchLength1
--    nLeaves2 branchLength2
--    ....
formatNChildSumStat :: NChildSumStat -> T.Text
formatNChildSumStat s = T.toStrict.  B.toLazyText . mconcat $ map formatNChildSumStatLine s

-- | Internal function.
formatNChildSumStatLine :: BrLnNChildren -> B.Builder
formatNChildSumStatLine (l, n) = B.decimal n
                                 <> B.singleton ' '
                                 <> B.realFloat l
                                 <> B.singleton '\n'
