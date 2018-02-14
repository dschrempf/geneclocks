{- |
   Description :  Species trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of species trees.

-}

module SpeciesTree
  ( SName
  , STree
  ) where

import           Tree
import qualified Data.Text as T

-- | Species name.
type SName = T.Text

-- | A species tree is just a binary tree with text as node labels and branch lengths.
type STree = Tree SName Double
