{- |
   Module      :  Geneclocks.Tree.Gene
   Description :  Gene trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of gene trees. A gene tree describes the relationship of genes of the
same gene family. Each gene can be assigned to a locus. Since this information
is not available, I will not use gene trees at the moment. Rater, I will work
with "GeneIndividual" trees, which combine the notion of gene and individual.

-}

module Geneclocks.Tree.Gene
  ( GLabel(..)
  , GState(..)
  , GTree(..)
  ) where

import Geneclocks.Tree.Phylo
import Geneclocks.Tree.Locus

-- | Gene name.
newtype GLabel a = GLabel a

-- | A gene has a name and belongs to a locus.
newtype GState a = GState (GLabel a, LLabel a)

-- | A gene tree is a binary tree, but genes have not only names but are also
-- associated to species.
--
-- TODO: Node type.
newtype GTree a b c = GTree (PhyloTree (GState a) b c)
