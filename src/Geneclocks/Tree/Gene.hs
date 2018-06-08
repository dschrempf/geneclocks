-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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
-}

module Geneclocks.Tree.Gene
  ( GName(..)
  , GState(..)
  , gStateToGName
  , gStateToIName
  , gStateToSName
  , GNodeType(..)
  , duplicationString
  , GTree
  , gRootNodeGName
  , gRootNodeIName
  , gRootNodeSName
  , gAgree
  ) where

import qualified Data.Set                   as S
import           Geneclocks.Tools
import           Geneclocks.Tree.Individual
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics               (Generic)

-- | Gene name.
newtype GName a = GName a deriving (Eq, Ord)

-- | A gene has a name and belongs to an individual.
newtype GState a = GState (GName a, IState a) deriving (Eq, Ord)

-- | Extract gene name.
gStateToGName :: GState a -> GName a
gStateToGName (GState (n, _)) = n

-- | Extract individual name.
gStateToIName :: GState a -> IName a
gStateToIName (GState (_, IState iS)) = fst iS

-- | Extranct species name.
gStateToSName :: GState a -> SName a
gStateToSName (GState (_, IState iState)) = snd iState

-- | Node types for genes (on individuals (on species)).
data GNodeType a =
  -- | Degree two node. The underlying species merges, and 'SName' changes to
  -- the one of the ancestor (if going backwards).
  GSCoalescence
  -- | Individuals coalesce. A coalescence of individuals changes the 'IName'.
  -- This event involves at least two daughter lineages.
  | GICoalescence
  -- | Degree two node; a coalescent event of individuals without an observed
  -- partner gene lineage, because it was lost somewhere on the tree. The
  -- problem is, that we need to record the individual that this coalescent
  -- event happened with. TODO: Proper, unambiguous handling of daughter
  -- individual states. Changes 'IName'.
  | GICoalescenceLoss (IState a)
  -- | The daughters arise due to a duplication. A duplication changes the
  -- 'GName', but leaves 'SName' and 'IName' unchanged.
  | GDuplication
  | GExtinct -- ^ Extinct leaf (probably not necessary, but why not).
  | GExtant  -- ^ Extant leaf.
  deriving (Eq, Read, Generic)

-- | Denote duplication.
duplicationString :: String
duplicationString = wrap 'D'

instance Show (GNodeType a) where
  show GSCoalescence         = speciationString
  show GICoalescence         = coalescentString
  show (GICoalescenceLoss _) = coalescentString
  show GDuplication          = duplicationString
  show GExtinct              = extinctString
  show GExtant               = extantString

instance (Eq a) => NodeType (GNodeType a) where
  extant GExtant = True
  extant _       = False
  extinct GExtinct = True
  extinct _        = False
  defaultExternal  = GExtant
  defaultInternal  = GDuplication

-- | A gene individual tree.
type GTree a b = PhyloTree (GState a) b (GNodeType a)

-- | Extract name of gene of root node.
gRootNodeGName :: GTree a b -> GName a
gRootNodeGName = gStateToGName . rootNodeState

-- | Extract name of individual of root node.
gRootNodeIName :: GTree a b -> IName a
gRootNodeIName = gStateToIName . rootNodeState

-- | Extract name of species of root node.
gRootNodeSName :: GTree a b -> SName a
gRootNodeSName = gStateToSName . rootNodeState

-- | Check if a gene individual tree is valid.
--
-- Performed checks:
--
--   - Validity of gene individual tree.
--
--   - Validity of species tree.
--
--   - No coalescence of any pair of genes before the respective species
--     coalesce.
--
--   - (Number of coalescent events) == (Number of genes - 1).
--
--   - Each gene has to coalesce at least once. THIS IS WRONG. Only, if I
--     introduce degree two nodes in all lineages at the time slice of each
--     coalescence.
-- TODO.
gAgree :: (Show a, Eq a, Ord a, Show b, Ord b, Num b) => GTree a b -> ITree a b -> STree a b -> Bool
gAgree g i s = valid g && iAgree i s && gCheckHeightsNSplits s (heightsNSplits g)

-- sCoalescencesAgree :: GITree a b -> STree a b -> Bool
-- sCoalescencesAgree g s = undefined
--   where hs = heightsNSplits g

-- | Check if the species tree agrees with the [(Height, Split)] list.
gCheckHeightsNSplits :: (Ord a, Ord b, Num b) => STree a b -> [(b, GTree a b)] -> Bool
gCheckHeightsNSplits s = all (gCheckHeightNSplit s)

-- TODO TODO TODO.
gCheckHeightNSplit :: (Ord a, Ord b, Num b) => STree a b -> (b, GTree a b) -> Bool
gCheckHeightNSplit s (h, g)
  -- Basically, we need to go through all different node types.
  | gt == GSCoalescence  = mrcaHeight daughterSpecies s == Just h &&
                          nDaughters == 1 &&
                          S.elemAt 0 daughterSpecies /= gs
  | gt == GICoalescence  = isSingleton daughterSpecies &&
                          isPairwiseDistinct daughterIndividualL &&
                          gi `S.notMember` daughterIndividuals
  | GICoalescenceLoss (IState (_, sL)) <- gt  = isSingleton (S.insert sL daughterSpecies) &&
                                               nDaughters == 1
  | gt == GDuplication  = isSingleton daughterIndividuals && isSingleton daughterSpecies
  | external gt         = True
  | otherwise           = error "GINodeType did not patter match. Weird."
  -- TODO: Check that coalescent events happen at the same time.
  where
    gs = gRootNodeSName g
    gi = gRootNodeIName g
    gt = rootNodeType g
    gDaughters = subForest g
    nDaughters = length gDaughters
    daughterSpecies = S.fromList $ map (gStateToSName . rootNodeState) gDaughters
    daughterIndividualL = map (gStateToIName . rootNodeState) gDaughters
    daughterIndividuals = S.fromList daughterIndividualL

-- TODO: Test this!

