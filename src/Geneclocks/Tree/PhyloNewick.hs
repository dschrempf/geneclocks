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
  ( toNewickString
  , toNewickText
  , toNewickIntegral
  , toNewickWith
  , toNewickWithBuilder
  ) where

import           Data.List                        (intersperse)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as T (toStrict)
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Geneclocks.Tools                 as Tools
import           Geneclocks.Tree.Phylo

-- | Convert a phylogenetic tree with text node labels into a Newick text object.
toNewickText :: (RealFloat b) => PhyloTree T.Text b -> T.Text
toNewickText = toNewickWith id Tools.realFloatToText

-- | Convert a phylogenetic tree with string node labels into a Newick text object.
toNewickString :: (RealFloat b) => PhyloTree String b -> T.Text
toNewickString = toNewickWith T.pack Tools.realFloatToText

-- | Convert a phylogenetic tree with integral node labels into a Newick text
-- object. This function is preferable because it uses the text builder and is
-- much faster.
toNewickIntegral :: (Integral a, RealFloat b) => PhyloTree a b -> T.Text
toNewickIntegral t = T.toStrict $ B.toLazyText $ toNewickWithBuilder B.decimal B.realFloat t

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided functions to convert node states and branches to text objects.
-- See also Biobase.Newick.Export.
toNewickWith :: (a -> T.Text) -> (b -> T.Text)-> PhyloTree a b -> T.Text
toNewickWith f g t = go t `T.append` T.pack ";"
  where
    go (Node s [])   = lbl s
    go (Node s ts)   = T.pack "(" `T.append`
                         T.concat (intersperse (T.pack ",") $ map go ts)
                         `T.append` T.pack ")" `T.append` lbl s
    lbl (Info s l _) = f s `T.append`
                       T.pack ":" `T.append` g l

-- | General conversion of a tree into a Newick string in form of a text object.
-- Use provided text builders to convert node states and branches to text
-- objects.
toNewickWithBuilder :: (a -> B.Builder) -> (b -> B.Builder) -> PhyloTree a b -> B.Builder
toNewickWithBuilder f g t = go t `mappend` B.singleton ';'
  where
    go (Node s [])   = lbl s
    go (Node s ts)   = B.singleton '(' `mappend`
                         mconcat (intersperse (B.singleton ',') $ map go ts)
                         `mappend` B.singleton ')' `mappend` lbl s
    lbl (Info s l _) = f s `mappend` B.singleton ':' `mappend` g l
