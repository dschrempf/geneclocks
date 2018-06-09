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
  ( SName(..)
  , STree
  , SNodeType(..)
  , speciationString
  -- , speciations
  ) where

-- import           Control.DeepSeq       (NFData)
import           Geneclocks.Tree.Phylo
import           GHC.Generics          (Generic)

-- | Species name.
newtype SName a = SName {sName :: a} deriving (Eq, Ord, Read, Generic, Enum, Num, Real, Integral)

instance Show a => Show (SName a) where
  -- show (SName s) = wrap 'S' ++ show s
  show (SName s) = 'S' : show s

-- | Node type of a phylogenetic tree. Technically, the type 'Internal' is not
-- necessary because it can be deduced from the tree. However, it is convenient
-- to save the type in this way.
data SNodeType = SCoalescent         -- ^ Internal node.
               | SExtant              -- ^ Extant leaf.
               | SExtinct             -- ^ Extinct leaf.
               deriving (Eq, Read, Generic)

instance NodeType SNodeType where
  extant   SExtant      = True
  extant   _            = False
  extinct  SExtinct     = True
  extinct  _            = False
  internal SCoalescent = True
  internal _            = False
  defaultExternal       = SExtant
  defaultInternal       = SCoalescent

-- | Denote speciation.
speciationString :: String
speciationString = wrap 'S'

instance Show SNodeType where
  show SCoalescent = speciationString
  show SExtant      = existenceString
  show SExtinct     = extinctionString

-- | A species tree is a tree with 'SName's as node states, and 'SNodeType's as
-- node types. The node state is called name. This may be subject to change.
-- However, e.g., for an individual tree, the state consists of both, the
-- individual name and the species name.
type STree a b = PhyloTree (SName a) b SNodeType

-- -- | Get all speciations (Height, Tree) on a species tree.
-- speciations :: (Num b, Ord b) => STree a b -> [(b, STree a b)]
-- speciations s = filter (\(_, t) -> internal . nodeType . rootNode $ t) (heightsNSplits s)
