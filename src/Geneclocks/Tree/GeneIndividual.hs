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
  , GITree(..)
  ) where

import           Control.DeepSeq
import           Geneclocks.Tree.Phylo
import qualified Geneclocks.Tree.Species as S
import           GHC.Generics            (Generic)

-- | Individual name.
newtype ILabel a = ILabel a

-- | State of individual.
-- TODO: S.SLabel??
newtype IState a = IState (ILabel a, S.SLabel a)

-- | Gene name.
newtype GLabel a = GLabel a

-- | A gene has a name and belongs to an individual, which belongs to a species.
newtype GState a = GIState (GLabel a, IState a)

-- | Node types for genes on individuals.
data GNode = Duplication       -- The daughters arise due to a duplication.
            | Coalescence       -- The daughters arise because the individuals
                                -- reproduce (or coalesce).
            | Extant            -- Extant leaf.
            | Extinct           -- Extinct leaf (probably not necessary, but why not).
            deriving (Eq, Read, Show, Generic, NFData)

instance NodeType GNode where
  extant Extant = True
  extant _      = False
  extinct Extinct = True
  extinct _       = False
  defaultExternal = Extant
  defaultInternal = Duplication

-- | A gene individual tree.
newtype GITree a b = GITree (PhyloTree (GState a) b GNode)

-- | Check if a gene individual tree is valid.
-- valid :: GITree a b -> Bool

