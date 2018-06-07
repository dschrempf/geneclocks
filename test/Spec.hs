module Main
  where

import qualified GeneTree as G
import qualified Phylo    as P

main :: IO ()
main = do
  P.performTests
  G.performTests
