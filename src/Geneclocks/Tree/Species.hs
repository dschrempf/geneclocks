-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , STree
  , SNodeType(..)
  , speciationString
  -- , speciations
  ) where

-- import           Control.DeepSeq       (NFData)
import           Geneclocks.Tree.Phylo
import           GHC.Generics          (Generic)

-- | Species name.
newtype SLabel a = SLabel a deriving (Eq, Ord, Read, Generic, Enum, Num, Real, Integral)

instance Show a => Show (SLabel a) where
  show (SLabel s) = "[S]" ++ show s

-- | Node type of a phylogenetic tree. Technically, the type 'Internal' is not
-- necessary because it can be deduced from the tree. However, it is convenient
-- to save the type in this way.
data SNodeType = SCoalescence         -- ^ Internal node.
               | SExtant              -- ^ Extant leaf.
               | SExtinct             -- ^ Extinct leaf.
               deriving (Eq, Read, Generic)

instance NodeType SNodeType where
  extant   SExtant      = True
  extant   _            = False
  extinct  SExtinct     = True
  extinct  _            = False
  internal SCoalescence = True
  internal _            = False
  defaultExternal       = SExtant
  defaultInternal       = SCoalescence

-- | Denote speciation.
speciationString :: String
speciationString = wrap 'S'

instance Show SNodeType where
  show SCoalescence = speciationString
  show SExtant      = extantString
  show SExtinct     = extinctString

-- | A species tree is just a binary tree with text as node labels and branch lengths.
type STree a b = PhyloTree (SLabel a) b SNodeType

-- -- | Get all speciations (Height, Tree) on a species tree.
-- speciations :: (Num b, Ord b) => STree a b -> [(b, STree a b)]
-- speciations s = filter (\(_, t) -> internal . nodeType . rootNode $ t) (heightsNSplits s)
