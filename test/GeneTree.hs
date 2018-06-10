module GeneTree
  where

import           Geneclocks.Tree.Gene
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
  Node (PhyloLabel (SName 0) 1.0 SCoalescent)
  [ Node (PhyloLabel (SName 1) 0.9 SCoalescent)
    [ Node (PhyloLabel (SName 3) 0.7 SExtant) []
    , Node (PhyloLabel (SName 4) 0.7 SExtant) []]
  , Node (PhyloLabel (SName 2) 1.6 SExtant) []]

individualTree :: ITree Int Double
individualTree =
  Node (PhyloLabel (iStateFromInts 0 0) 0.9 ICoalescent)
  [ Node (PhyloLabel (iStateFromInts 1 0) 0.1 ISCoalescent)
    [Node (PhyloLabel (iStateFromInts 1 1) 0.8 ICoalescent)
     [ Node (PhyloLabel (iStateFromInts 3 1) 0.1 ISCoalescent)
       [Node (PhyloLabel (iStateFromInts 3 3) 0.7 IExtant) []]
     , Node (PhyloLabel (iStateFromInts 4 1) 0.1 ISCoalescent)
       [Node (PhyloLabel (iStateFromInts 4 4) 0.7 IExtant) []]]]
  , Node (PhyloLabel (iStateFromInts 2 0) 0.1 ISCoalescent)
    [Node (PhyloLabel (iStateFromInts 2 2) 1.6 IExtant) []]]

geneTree :: GTree Int Double
geneTree =
  Node (PhyloLabel (gStateFromInts 0 0 0) 0.9 GICoalescent)
  [ Node (PhyloLabel (gStateFromInts 1 1 0) 0.1 GSCoalescent)
    [Node (PhyloLabel (gStateFromInts 1 1 1) 0.8 GICoalescent)
     [ Node (PhyloLabel (gStateFromInts 3 3 1) 0.1 GSCoalescent)
       [Node (PhyloLabel (gStateFromInts 3 3 3) 0.7 GExtant) []]
     , Node (PhyloLabel (gStateFromInts 4 4 1) 0.1 GSCoalescent)
       [Node (PhyloLabel (gStateFromInts 4 4 4) 0.7 GExtant) []]]]
  , Node (PhyloLabel (gStateFromInts 2 2 0) 0.1 GSCoalescent)
    [Node (PhyloLabel (gStateFromInts 2 2 2) 1.6 GExtant) []]]

performTests :: IO ()
performTests = do
  announce "Agreement of an individual and species tree."
  report "Species tree."
  reportText $ toNewick speciesTree
  report "Individual tree."
  reportText $ toNewick individualTree
  either error
    (\_ -> report "Individual and species trees do agree.")
    (assertISAgreement individualTree speciesTree)

  announce "Agreement of gene tree with individual and species trees."
  report "Gene tree."
  reportText $ toNewick geneTree
  either error
    (\_ -> report "Gene, individual and species trees do agree")
    (assertGISAgreement geneTree individualTree speciesTree)
