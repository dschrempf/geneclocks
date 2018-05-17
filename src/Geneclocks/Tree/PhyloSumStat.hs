{- |
Module      :  Geneclocks.Tree.PhyloSumStat
Description :  Summary statistics for phylogenetic trees.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May 17 14:05:45 2018.

-}

module Geneclocks.Tree.PhyloSumStat
  ( BrLnNChildren
  , NChildSumStat
  , formatNChildSumStat
  ) where

import           Data.Monoid ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as T (toStrict)
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

-- This may be too specific, but I only change it if necessary. E.g., use types
-- a (for node labels) and b (for branch lengths).

-- | Pair of branch length with number of extant children.
type BrLnNChildren = (Double, Int)

-- | Possible summary statistic of phylogenetic trees. A list of tuples
-- (BranchLength, NumberOfExtantChildrenBelowThisBranch).
type NChildSumStat = [BrLnNChildren]

-- | Format the summary statistics in the following form:
-- @
--    nLeaves1 branchLength1
--    nLeaves2 branchLength2
--    ....
formatNChildSumStat :: NChildSumStat -> T.Text
formatNChildSumStat s = T.toStrict.  B.toLazyText . mconcat $ map formatNChildSumStatLine s

formatNChildSumStatLine :: BrLnNChildren -> B.Builder
formatNChildSumStatLine (l, n) = B.decimal n
                                 <> B.singleton ' '
                                 <> B.realFloat l
                                 <> B.singleton '\n'

-- TODO: Separate simulation of tree and conversion to, e.g., BrLnNChildren
-- (both is now done in PointProcess.hs). However, this may be slower.
