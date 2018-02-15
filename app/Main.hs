module Main where

import System.Random.MWC
import PointProcess
import Control.Monad
import PhyloTree
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  g <- create
  trs <- replicateM 100 (simulateReconstructedTree 20 2.0 1.0 0.9 g)
  T.putStr $ T.concat $ map toNewickInt trs
