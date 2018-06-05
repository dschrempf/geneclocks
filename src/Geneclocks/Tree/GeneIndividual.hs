{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
   Module      :  Geneclocks.Tree.GeneIndividual
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

-}

module Geneclocks.Tree.GeneIndividual
  ( ILabel(..)
  , IState(..)
  , GLabel(..)
  , GState(..)
  , GNode(..)
  , GITree
  , agree
  ) where

import           Control.DeepSeq
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics            (Generic)

-- | Individual name.
newtype ILabel a = ILabel a

-- | State of individual.
newtype IState a = IState (ILabel a, SLabel a)

-- | Gene name.
newtype GLabel a = GLabel a

-- | A gene has a name and belongs to an individual, which belongs to a species.
newtype GState a = GIState (GLabel a, IState a)

-- | Node types for genes on individuals.
data GNode = GDuplication  -- The daughters arise due to a duplication.
            | GCoalescence -- The daughters arise because the individuals
                           -- reproduce (or coalesce).
            | GExtant      -- Extant leaf.
            | GExtinct     -- Extinct leaf (probably not necessary, but why not).
            deriving (Eq, Read, Show, Generic, NFData)

instance NodeType GNode where
  extant GExtant   = True
  extant _         = False
  extinct GExtinct = True
  extinct _        = False
  defaultExternal  = GExtant
  defaultInternal  = GDuplication

-- | A gene individual tree.
type GITree a b = PhyloTree (GState a) b GNode

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
--     introduce degree two nodes at the time slice of coalescence, at all
--     lineages.

agree :: GITree a b -> STree a b -> Bool
agree gi s = valid gi && valid s
