{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
  ( SLabel(..)
  , STree(..)
  , SNode(..)
  ) where

import           Control.DeepSeq       (NFData)
import           Geneclocks.Tree.Phylo
import           GHC.Generics          (Generic)

-- | Species name.
newtype SLabel a = SLabel a

-- | Node type of a phylogenetic tree. Technically, the type 'Internal' is not
-- necessary because it can be deduced from the tree. However, it is convenient
-- to save the type in this way.
data SNode = Internal            -- ^ Internal node.
           | Extant              -- ^ Extant leaf.
           | Extinct             -- ^ Extinct leaf.
           deriving (Eq, Read, Show, Generic, NFData)

instance NodeType SNode where
  extant   Extant = True
  extant   _      = False
  extinct  Extinct = True
  extinct  _       = False
  internal Internal = True
  internal _        = False
  defaultExternal   = Extant
  defaultInternal   = Internal

-- | A species tree is just a binary tree with text as node labels and branch lengths.
newtype STree a b = STree (PhyloTree (SLabel a) b SNode)
