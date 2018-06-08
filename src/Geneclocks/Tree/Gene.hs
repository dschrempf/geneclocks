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
    (i.e., free recombination, and some other assumptions)

TODO: Idea, explicitely use an individual tree. This eases testing of the
validity of a gene tree quite a lot.

-}

module Geneclocks.Tree.Gene
  ( GLabel(..)
  , GState(..)
  , GNodeType(..)
  , duplicationString
  , GTree
  , gAgree
  ) where

-- import           Control.DeepSeq
import qualified Data.Set                   as S
import           Geneclocks.Tools
import           Geneclocks.Tree.Individual
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics               (Generic)

-- | Gene name.
newtype GLabel a = GLabel a deriving (Eq, Ord)

-- | A gene has a name and belongs to an individual.
newtype GState a = GState (GLabel a, IState a) deriving (Eq, Ord)

-- gLabel :: GState a -> GLabel a
-- gLabel (GState (gl, _)) = gl

gStateToILabel :: GState a -> IName a
gStateToILabel (GState (_, IState iState)) = fst iState

gStateToSLabel :: GState a -> SName a
gStateToSLabel (GState (_, IState iState)) = snd iState

-- | Node types for genes (on individuals (on species)).

-- XXX: Before calculation of the likelihood for substitution models, all degree
-- two nodes have to be removed anyways... complicated.

-- Should we just ignore degree two nodes? But then, the notion that each gene
-- can be assigned to an individual and a species is difficult.

data GNodeType a =
  -- | By definition a degree two node. The underlying species merges, and
  -- 'SLabel' changes to the one of the ancestor (if going backwards). XXX: Is
  -- this necessary?
  GSCoalescence
  -- | Individuals coalesce. A coalescence of individuals changes the 'ILabel'.
  -- This event involves at least two daughter lineages.
  | GICoalescence
  -- | By definition a degree two node; a coalescent event of individuals
  -- without an observed partner gene lineage, because it was lost somewhere on
  -- the tree. The problem is, that we need to record the individual that this
  -- coalescent event happened with.
  | GICoalescenceLoss (IState a)
  -- | The daughters arise due to a duplication. A duplication changes the
  -- 'GLabel', but leaves 'SLabel' and 'ILabel' unchanged.
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
    gn = rootLabel g
    gs = gStateToSLabel . state $ gn
    gi = gStateToILabel . state $ gn
    gt = nodeType gn
    gDaughters = subForest g
    nDaughters = length gDaughters
    daughterSpecies = S.fromList $ map (gStateToSLabel . rootNodeState) gDaughters
    daughterIndividualL = map (gStateToILabel . rootNodeState) gDaughters
    daughterIndividuals = S.fromList daughterIndividualL

-- TODO: Test this!

-- TODO: What can be done to avoid re-computation of heights, leaf sets, and so
--       on?
