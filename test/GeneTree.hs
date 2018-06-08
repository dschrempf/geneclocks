module GeneTree
  where

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
  Node (PhyloLabel (SName 0) 1.0 SCoalescence)
  [ Node (PhyloLabel (SName 1) 0.9 SCoalescence)
    [ Node (PhyloLabel (SName 3) 0.7 SExtant) []
    , Node (PhyloLabel (SName 4) 0.7 SExtant) []]
  , Node (PhyloLabel (SName 2) 1.6 SExtant) []]

individualTree :: ITree Int Double
individualTree =
  Node (PhyloLabel (iStateFromInts 0 0) 0.9 ICoalescence)
  [ Node (PhyloLabel (iStateFromInts 1 0) 0.1 ISCoalescence)
    [Node (PhyloLabel (iStateFromInts 1 1) 0.8 ICoalescence)
     [ Node (PhyloLabel (iStateFromInts 3 1) 0.1 ISCoalescence) [Node (PhyloLabel (iStateFromInts 3 3) 0.7 IExtant) []]
     , Node (PhyloLabel (iStateFromInts 4 1) 0.1 ISCoalescence) [Node (PhyloLabel (iStateFromInts 4 4) 0.7 IExtant) []]]]
  , Node (PhyloLabel (iStateFromInts 2 0) 0.1 ISCoalescence) [Node (PhyloLabel (iStateFromInts 2 2) 1.6 IExtant) []]]

performTests :: IO ()
performTests = do
  announce "Agreement of an individual and species tree."
  report "Species tree."
  reportText $ toNewick speciesTree
  report "Individual tree."
  reportText $ toNewick individualTree
  if iAgree individualTree speciesTree
    then report "Individual and species trees do agree."
    else error "Individual and species tree should agree."
