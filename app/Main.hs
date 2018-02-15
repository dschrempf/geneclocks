module Main where

import System.Random.MWC
import PointProcess
import Control.Monad
import PhyloTree
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let nTrees = 100
      n      = 1000
      t      = 2.0
      l      = 1.0
      m      = 9.0
  g <- createSystemRandom
  trs <- replicateM nTrees (simulateReconstructedTree n t l m g)
  T.putStr $ T.concat $ map toNewickInt trs
