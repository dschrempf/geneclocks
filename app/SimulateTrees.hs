module Main where

import           Control.Concurrent (getNumCapabilities, myThreadId, threadCapability)
import           Control.Concurrent.Async (replicateConcurrently)
import           Control.Monad (when, replicateM)
import           Data.Semigroup               ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative
import           PhyloTree (toNewickInt)
import           PointProcess (simulateReconstructedTree)
import           System.Random.MWC (createSystemRandom)

data Args = Args
  { nTrees    :: Int    -- ^ Simulated trees per core.
  , nSamples  :: Int    -- ^ Species or samples.
  , height    :: Double -- ^ Tree height (time to origin)>
  , lambda    :: Double -- ^ Birth rate.
  , mu        :: Double -- ^ Death rate.
  , verbosity :: Bool   -- ^ Verbosity.
  }

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

nTreeOpt :: Parser Int
nTreeOpt = option auto
  ( long "nTrees"
    <> short 't'
    <> metavar "INT"
    <> value 100
    <> showDefault
    <> help "Number of trees" )

nSamplesOpt :: Parser Int
nSamplesOpt = option auto
  ( long "nSamples"
    <> short 'n'
    <> metavar "INT"
    <> value 1000
    <> showDefault
    <> help "Number of samples per tree" )

treeHeightOpt :: Parser Double
treeHeightOpt = option auto
  ( long "height"
    <> short 'H'
    <> metavar "DOUBLE"
    <> value 2.0
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
verbosityOpt = option auto
  ( long "verbosity"
    <> short 'v'
    <> metavar "BOOL"
    <> value False
    <> showDefault
    <> help "Verbosity" )

main :: IO ()
main = do
  args <- parseArgs
  let t = nTrees args
      s = nSamples args
      h  = height args
      l  = lambda args
      m  = mu args
      v  = verbosity args
  -- Use one capability for now. Seems to be more stable.
  c <- getNumCapabilities
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
