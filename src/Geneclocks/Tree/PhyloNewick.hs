{- |
Module      :  Geneclocks.Tree.PhyloNewick
Description :  Convert phylogenetic trees to Newick format.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May 17 14:01:12 2018.

-}

module Geneclocks.Tree.PhyloNewick
  ( toNewick
  , toNewickIntegral
  , toNewickWithBuilder
  ) where

import           Data.List                  (intersperse)
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as T (toStrict)
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import           Data.Tree
import qualified Geneclocks.Tools           as Tools
import           Geneclocks.Tree.Phylo

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided functions to convert node labels to node and branch length text
-- objects. See also Biobase.Newick.Export.
toNewick :: (PhyloLabel a) => Tree a -> T.Text
toNewick t = T.toStrict $ B.toLazyText $ go t <> B.singleton ';'
  where
    go (Node l [])   = lbl l
    go (Node l ts)   = B.singleton '('
                       <> mconcat (intersperse (B.singleton ',') $ map go ts)
                       <> B.singleton ')'
                       <> lbl l
    lbl l = nodeBuilder l
            <> B.singleton ':'
            <> brLnBuilder l

-- | Convert a phylogenetic tree with integral node labels into a Newick text
-- object. This function is preferable because it uses the text builder and is
-- much faster.
toNewickIntegral :: (Integral a, RealFloat b) => PhyloTree a b c -> T.Text
toNewickIntegral t = T.toStrict $ B.toLazyText $ toNewickWithBuilder B.decimal Tools.realFloatBuilder t

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided text builders to convert node states and branches to text
-- objects.
toNewickWithBuilder :: (a -> B.Builder) -> (b -> B.Builder) -> PhyloTree a b c -> B.Builder
toNewickWithBuilder f g t = go t `mappend` B.singleton ';'
  where
    go (Node s [])   = lbl s
    go (Node s ts)   = B.singleton '(' `mappend`
                         mconcat (intersperse (B.singleton ',') $ map go ts)
                         `mappend` B.singleton ')' `mappend` lbl s
    lbl (Info s l _) = f s `mappend` B.singleton ':' `mappend` g l
