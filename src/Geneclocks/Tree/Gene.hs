{- |
   Module      :  Geneclocks.Tree.Gene
   Description :  Gene trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of gene trees.

-}

module Geneclocks.Tree.Gene
  ( GName(..)
  , GState(..)
  , GTree(..)
  ) where

import Geneclocks.Tree.Phylo
import qualified Data.Text as T
import Geneclocks.Tree.Locus

-- | Gene name.
newtype GName = GName T.Text

-- | A gene has a name and belongs to a locus.
newtype GState = GState (GName, LName)

-- | A gene tree is a binary tree, but genes have not only names but are also
-- associated to species.
newtype GTree = GTree (Tree GState)
