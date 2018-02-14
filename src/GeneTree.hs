{- |
   Description :  Gene trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of gene trees.

-}

module GeneTree
  ( GName
  , GState
  , GTree
  ) where

import Tree
import qualified Data.Text as T
import SpeciesTree

-- | Gene name.
type GName = T.Text

-- | A gene has a name and belongs to a species.
type GState = (GName, SName)

-- | A gene tree is a binary tree, but genes have not only names but are also
-- associated to species.
type GTree = Tree GState Double
