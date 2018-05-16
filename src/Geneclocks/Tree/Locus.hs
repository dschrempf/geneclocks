{- |
   Module      :  Geneclocks.Tree.Locus
   Description :  Locus trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of locus trees.

-}

module Geneclocks.Tree.Locus
  ( LName(..)
  , LState(..)
  , LTree(..)
  ) where

import Geneclocks.Tree.Phylo
import qualified Data.Text as T
import Geneclocks.Tree.Species

-- | Locus name.
newtype LName = LName T.Text

-- | A locus has a name and belongs to a species.
newtype LState = LState (LName, SName)

-- | A locus tree is a tree, but loci have not only names but are also
-- associated to species.
newtype LTree = LTree (Tree LState)
