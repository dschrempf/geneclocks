{- |
   Description :  Simulate reconstructed trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Tue Feb 27 17:27:16 2018.

See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.

TODO: Allow to specify seed value.

TODO: lambda ~ mu.

-}

module Main where

import           Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import           Control.Concurrent.Async (replicateConcurrently)
import           Control.Monad (when, replicateM, unless)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           PhyloTree (toNewickInt)
import           PointProcess (simulateReconstructedTree)
import qualified System.Environment as Sys
import           System.Random.MWC (createSystemRandom)

data Args = Args
  { nTrees    :: Int    -- ^ Simulated trees.
  , nSamples  :: Int    -- ^ Species or samples.
  , height    :: Double -- ^ Tree height (time to origin)>
  , lambda    :: Double -- ^ Birth rate.
  , mu        :: Double -- ^ Death rate.
  , verbosity :: Bool   -- ^ Verbosity.
  , quiet     :: Bool   -- ^ Be quiet?
  }

reportArgs :: Args -> String
reportArgs (Args t s h l m v q) =
  unlines [ "Number of simulated trees: " ++ show t
          , "Number of species per tree: " ++ show s
          , "Height of trees: " ++ show h
          , "Birth rate: " ++ show l
          , "Death rate: " ++ show m
          , "Verbosity: " ++ show v
          , "Quiet: " ++ show q ]

-- | The impure IO action that reads the arguments and prints out help if
  -- needed.
parseArgs :: IO Args
parseArgs = execParser $
  info (helper <*> argsParser)
  (fullDesc
    <> header "Simulate reconstructed trees"
    <> progDesc "Simulate reconstructed trees using the point process." )

argsParser :: Parser Args
argsParser = Args
  <$> nTreeOpt
  <*> nSamplesOpt
  <*> treeHeightOpt
  <*> lambdaOpt
  <*> muOpt
  <*> verbosityOpt
  <*> quietOpt

nTreeOpt :: Parser Int
nTreeOpt = option auto
  ( long "nTrees"
    <> short 't'
    <> metavar "INT"
    <> value 10
    <> showDefault
    <> help "Number of trees" )

nSamplesOpt :: Parser Int
nSamplesOpt = option auto
  ( long "nSamples"
    <> short 'n'
    <> metavar "INT"
    <> value 5
    <> showDefault
    <> help "Number of samples per tree" )

treeHeightOpt :: Parser Double
treeHeightOpt = option auto
  ( long "height"
    <> short 'H'
    <> metavar "DOUBLE"
    <> value 1.0
    <> showDefault
    <> help "Tree height" )


lambdaOpt :: Parser Double
lambdaOpt = option auto
  ( long "lambda"
    <> short 'l'
    <> metavar "DOUBLE"
    <> value 1.0
    <> showDefault
    <> help "Birth rate lambda" )

muOpt :: Parser Double
muOpt = option auto
  ( long "mu"
    <> short 'm'
    <> metavar "DOUBLE"
    <> value 0.9
    <> showDefault
    <> help "Death rate mu" )

verbosityOpt :: Parser Bool
verbosityOpt = switch
  ( long "verbosity"
    <> short 'v'
    <> showDefault
    <> help "Verbosity" )

quietOpt :: Parser Bool
quietOpt = switch
  ( long "quiet"
    <> short 'q'
    <> showDefault
    <> help "Be quiet" )

getCommandLineStr :: String -> [String] -> String
getCommandLineStr n as = unlines
  [ "Reconstructed trees simulator version 0.1.0.0."
  , "Command line: " ++ n ++ " " ++ unwords as ]

newSection :: String -> String
newSection h = unlines
  [ ""
  , "-- " ++ h ]

main :: IO ()
main = do
  args <- parseArgs
  let t = nTrees args
      s = nSamples args
      h  = height args
      l  = lambda args
      m  = mu args
      v  = verbosity args
      q  = quiet args
  -- Use one capability for now. Seems to be more stable.
  c <- getNumCapabilities
  unless q $ do
    p <- Sys.getProgName
    a <- Sys.getArgs
    putStr $ getCommandLineStr p a
    putStr $ newSection "Arguments"
    putStr $ reportArgs args
    putStr $ newSection "Simulation"
  when v $ putStrLn $ "Number of used cores: " ++ show c
  let tCap = t `div` c
  trs <- replicateConcurrently c (simulateNTrees tCap s h l m v)
  T.putStr $ T.concat trs
  -- If the total number of trees is not divisible by the number of
  -- capabilities, we have to create some more trees.
  let re   = t `mod` c
  trsRe <- simulateNTrees re s h l m v
  T.putStr trsRe

simulateNTrees :: Int -> Int -> Double -> Double -> Double -> Bool -> IO T.Text
simulateNTrees nT nS t l m v = do
  when v reportCapability
  g <- createSystemRandom
  trs <- replicateM nT (simulateReconstructedTree nS t l m g)
  return $ T.unlines $ map toNewickInt trs

reportCapability :: IO ()
reportCapability = do
  i <- myThreadId
  (c, _) <- threadCapability i
  putStrLn $ "Running on core: " ++ show c
