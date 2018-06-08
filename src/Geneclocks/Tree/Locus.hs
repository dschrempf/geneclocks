{- |
   Module      :  Geneclocks.Tree.Locus
   Description :  Locus trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of locus trees. The concept of a locus tree is only needed, when
information about loci is available. That is, when we can assign each gene to a
locus. We do not have this information.

-}

module Geneclocks.Tree.Locus
  ( LName(..)
  , LState(..)
  , LTree
  ) where

import Geneclocks.Tree.Phylo
import Geneclocks.Tree.Species

-- | Locus name.
newtype LName a = LName a

-- | A locus has a name and belongs to a species.
newtype LState a = LState (LName a, SName a)

-- | A locus tree is a tree, but loci have not only names but are also
-- associated to species.
--
-- TODO: Node type.
type LTree a b c = PhyloTree (LState a) b c
