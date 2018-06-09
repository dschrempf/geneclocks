-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      :  Geneclocks.Tree.Gene
   Description :  Gene trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Gene individual trees combine the evolution of genes in individuals. A leaf is a
gene that belongs to an individual which belongs to a species. Since we do not
have data for different individuals, there will be only one individual per
species at the leaves of the tree. However, this notion of a gene belonging to
an individual which belongs to a species is important when species merge going
backwards in time. Because then, genes from different individuals (which will
belong to different species) will be present in the same ancestral species.

Pros:

  - It is relatively easy to write down the constraints that allow a
    reconciliation of the GI tree into the species tree.

Cons:

  - No recombination.

Questions:

  - Should the coalescent rate depend on the gene composition of the indivduals?

  - Should we just run a multispecies-like coalescent model for all of the genes
    (i.e., free recombination, and some other assumptions)?

TODO: What can be done to avoid re-computation of heights, leaf sets, and so on?

Ideas:
- Topology data type.
- Data tree data type (only branch lengths and node states).
-}

module Geneclocks.Tree.Gene
  ( GName(..)
  , GState(..)
  , gStateFromInts
  , gStateToGName
  , gStateToIState
  , gStateToIName
  , gStateToSName
  , GNodeType(..)
  , duplicationString
  , GTree
  , gRootNodeGName
  , gRootNodeIState
  , gRootNodeIName
  , gRootNodeSName
  , gAgree
  ) where

import           Data.Maybe
import qualified Data.Set                   as S
import           Geneclocks.Tools
import           Geneclocks.Tree.Individual
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics               (Generic)

-- | Gene name.
newtype GName a = GName {gName :: a} deriving (Eq, Ord)

-- | A gene has a name and belongs to an individual.
newtype GState a = GState {gState :: (GName a, IState a)} deriving (Eq, Ord)

instance Show a => Show (GState a) where
  -- show (GState (GName g, i)) = wrap 'G' ++ show g ++ show i
  show (GState (GName g, i)) = 'G' : show g ++ show i

gStateFromInts :: Int -> Int -> Int -> GState Int
gStateFromInts g i s = GState (GName g, iStateFromInts i s)

-- | Extract gene name.
gStateToGName :: GState a -> GName a
gStateToGName = fst . gState

-- | Extract individual state.
gStateToIState :: GState a -> IState a
gStateToIState = snd . gState

-- | Extract individual name.
gStateToIName :: GState a -> IName a
gStateToIName (GState (_, IState s)) = fst s

-- | Extranct species name.
gStateToSName :: GState a -> SName a
gStateToSName (GState (_, IState s)) = snd s

-- | Node types for genes (on individuals (on species)).
data GNodeType a =
  -- | Degree two node. The underlying species merges, and 'SName' changes to
  -- the one of the ancestor (if going backwards).
  GSCoalescent
  -- | Individuals coalesce. A coalescence of individuals changes the 'IName'
  -- and the 'GName'.
  | GICoalescent
  -- -- REMOVED FOR NOW.
  -- -- | Degree two node; a coalescent event of individuals without an observed
  -- -- partner gene lineage, because it was lost somewhere on the tree. Changes
  -- -- 'IName'.
  -- -- | GICoalescenceLoss
  -- | The daughters arise due to a duplication. A duplication changes the
  -- 'GName', but leaves 'SName' and 'IName' unchanged.
  | GDuplication
  | GLoss    -- ^ Extinct leaf (probably not necessary, but why not).
  | GExtant  -- ^ Extant leaf.
  deriving (Eq, Read, Generic)

-- | Denote duplication.
duplicationString :: String
duplicationString = wrap 'D'

instance Show (GNodeType a) where
  show GSCoalescent     = speciationString
  show GICoalescent     = coalescenceString
  show GDuplication      = duplicationString
  show GLoss             = extinctionString
  show GExtant           = existenceString

instance (Eq a) => NodeType (GNodeType a) where
  extant GExtant = True
  extant _       = False
  extinct GLoss = True
  extinct _     = False
  defaultExternal = GExtant
  defaultInternal = GDuplication

-- | A gene individual tree.
type GTree a b = PhyloTree (GState a) b (GNodeType a)

-- | Extract name of gene of root node.
gRootNodeGName :: GTree a b -> GName a
gRootNodeGName = gStateToGName . rootNodeState

-- | Extract state of individual of root node.
gRootNodeIState :: GTree a b -> IState a
gRootNodeIState =  gStateToIState . rootNodeState

-- | Extract name of individual of root node.
gRootNodeIName :: GTree a b -> IName a
gRootNodeIName = gStateToIName . rootNodeState

-- | Extract name of species of root node.
gRootNodeSName :: GTree a b -> SName a
gRootNodeSName = gStateToSName . rootNodeState

-- | Check if a gene tree agrees with given individual and species trees.
--
-- Performed checks:
--   - Validity and clock-likeness of gene tree.
--   - Heights have to be equal.
--   - Agreement of individual and species tree.
--   - Lots of other stuff (see code).
gAgree :: (Show a, Eq a, Ord a, Show b, Ord b, Num b) => GTree a b -> ITree a b -> STree a b -> Bool
gAgree g i s = valid g && clockLike g &&
               heightClockLike g == heightClockLike i &&
               iAgree i s &&
               gRootNodeIState g == rootNodeState i &&
               gCheckHeightsNSplits i (heightsNSplits g)

-- Check if the individual tree agrees with the [(Height, Split)] list.
gCheckHeightsNSplits :: (Show a, Ord a, Show b, Ord b, Num b) => ITree a b -> [(b, GTree a b)] -> Bool
gCheckHeightsNSplits i = all (gCheckHeightNSplit i)

-- Check the coalescence at the root of the tree.
gSpeciationAgrees :: (Ord a, Eq b) => GTree a b -> ITree a b -> Bool
gSpeciationAgrees g i = rootNodeType g == GSCoalescent &&
                        rootNodeType i == ISCoalescent &&
                        rootNodesAgreeWith (gStateToIState . state) state g i

-- Check the coalescence at the root of the tree.
gCoalescenceAgrees :: (Ord a, Eq b) => GTree a b -> ITree a b -> Bool
gCoalescenceAgrees g i = rootNodeType g == GICoalescent &&
                         rootNodeType i == ICoalescent &&
                         rootNodesAgreeWith (gStateToIState . state) state g i

gCheckHeightNSplit :: (Show a, Ord a, Show b, Ord b, Num b) => ITree a b -> (b, GTree a b) -> Bool
gCheckHeightNSplit i (h, g)
  | gNT == GSCoalescent  =
      -- Heights match.
      h == iH &&
      -- Speciation agrees.
      gSpeciationAgrees g iMrcaTr &&
      -- Force degree two node.
      rootDegree g == 2 &&
      -- Gene name does not change.
      gRootNodeGName g `elem` map gRootNodeGName gDs
  | gNT == GICoalescent  =
      -- Heights match.
      h == iH &&
      -- Coalescece agrees.
      gCoalescenceAgrees g iMrcaTr &&
      -- Gene names have to change.
      isPairwiseDistinct (gRootNodeGName g : gDGs)
  | gNT == GDuplication   =
      -- Ancestor and daughter individuals states have to be equal.
      isSingleton (S.insert gI gDIs) &&
      -- Ancestor and daughter genes have to be different.
      isPairwiseDistinct (gRootNodeGName g : gDGs)
  | external gNT          = gI `elem` map state (getLeaves i)
  | otherwise             = error "GINodeType did not pattern match. Weird."
  where
    gI = gRootNodeIState g
    gNT = rootNodeType g
    gDs = subForest g
    gDIs = S.fromList $ map gRootNodeIState gDs
    gDGs = map gRootNodeGName gDs
    (iH, iMrcaTr) = fromMaybe
                    (error $ "MRCA of " ++ show gDIs ++ " not present in individual tree " ++ show i)
                    (mrcaHeightNTree (S.singleton gI) i)
