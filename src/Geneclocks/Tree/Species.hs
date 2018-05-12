{- |
   Module      :  Geneclocks.Tree.Species
   Description :  Species trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Definition of species trees.

-}

module Geneclocks.Tree.Species
  ( SName(..)
  , STree(..)
  ) where

import           Geneclocks.Tree.Phylo
import qualified Data.Text as T

-- | Species name.
newtype SName = SName T.Text

-- | A species tree is just a binary tree with text as node labels and branch lengths.
newtype STree = STree (Tree SName)
