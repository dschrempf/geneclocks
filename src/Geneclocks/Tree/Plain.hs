{- |
Module      :  Geneclocks.Tree.Plain
Description :  A simple tree with branch lengths used for likelihood computations.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Jun 11 10:31:00 2018.

Tree data structure for likelihood computations. Should be as simple as
possible.

-}


module Geneclocks.Tree.Plain
  ( PlainTree
  ) where

import Geneclocks.Tree.Phylo

-- | A simple phylogenetic tree. Nodes have a state and a branch length but
-- that's it.
type PlainTree a b = PhyloTree a b ()
