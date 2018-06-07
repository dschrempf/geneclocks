module GeneTree
  where

import qualified Data.Text                   as T
import           Geneclocks.Tree.Individual
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.PhyloNewick
import           Geneclocks.Tree.Species
import           TestIO

-- Should be a clock-like great ape tree.
-- 3: H
-- 4: C
-- 2: G
speciesTree :: STree Int Double
speciesTree =
  Node (Info (SLabel 0) 1.0 SCoalescence)
  [ Node (Info (SLabel 1) 0.9 SCoalescence)
    [ Node (Info (SLabel 3) 0.7 SExtant) []
    , Node (Info (SLabel 4) 0.7 SExtant) []]
  , Node (Info (SLabel 2) 1.6 SExtant) []]

individualTree :: ITree Int Double
individualTree =
  Node (Info (iStateFromInts 0 0) 0.1 ICoalescence)
  [ Node (Info (iStateFromInts 1 0) 0.1 ISCoalescence)
    [Node (Info (iStateFromInts 1 1) 0.8 ICoalescence)
     [ Node (Info (iStateFromInts 3 1) 0.1 ISCoalescence) [Node (Info (iStateFromInts 3 3) 0.7 IExtant) []]
     , Node (Info (iStateFromInts 4 1) 0.1 ISCoalescence) [Node (Info (iStateFromInts 4 4) 0.7 IExtant) []]]]
  , Node (Info (iStateFromInts 2 0) 0.1 ISCoalescence) [Node (Info (iStateFromInts 2 2) 1.6 IExtant) []]]

performTests :: IO ()
performTests = do
  announce "Agreement of an individual and species tree."
  report "Species tree."
  report $ T.unpack . toNewick $ speciesTree
  report "Individual tree."
  report $ T.unpack . toNewick $ individualTree
  if iAgree individualTree speciesTree
    then report "Individual and species trees do agree."
    else error "Individual and species tree should agree."
