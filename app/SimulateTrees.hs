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
import           PhyloTree (toNewickInt, PhyloTree)
import           PointProcess ( simulateReconstructedTree
                              , simulateReconstructedTreeRandomHeight
                              , simulateBranchLengthNChildren
                              , simulateBranchLengthNChildrenRandomHeight)
import qualified System.Environment as Sys
import           System.Random.MWC (createSystemRandom)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

data Args = Args
  { nTrees    :: Int    -- ^ Simulated trees.
  , nLeaves   :: Int    -- ^ Number of leaves.
  , height    :: Maybe Double -- ^ Tree height (time to origin).
  , lambda    :: Double -- ^ Birth rate.
  , mu        :: Double -- ^ Death rate.
  , sumStat   :: Bool   -- ^ Only print summary statistics?
  , verbosity :: Bool   -- ^ Verbosity.
  , quiet     :: Bool   -- ^ Be quiet?
  }

reportArgs :: Args -> String
reportArgs (Args t n mH l m s v q) =
  unlines [ "Number of simulated trees: " ++ show t
          , "Number of leaves per tree: " ++ show n
          , "Height of trees: " ++ hStr
          , "Birth rate: " ++ show l
          , "Death rate: " ++ show m
          , "Summary statistics only: " ++ show s
          , "Verbosity: " ++ show v
          , "Quiet: " ++ show q ]
  where hStr = case mH of Nothing -> "Random"
                          Just h  -> show h

-- | The impure IO action that reads the arguments and prints out help if
  -- needed.
parseArgs :: IO Args
parseArgs = execParser $
  info (helper <*> argsParser)
  (fullDesc
    <> header "Simulate reconstructed trees"
    <> progDesc desc
    <> footerDoc remarks )
  where
    desc = "Simulate reconstructed trees using the point process. See Gernhard, T. (2008). The conditioned reconstructed process. Journal of Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005"
    remarks = Just $ foldl1 (Doc.<$>) (map Doc.text strs)
    strs    = [ "If no tree height is given, the heights will be randomly drawn from the expected"
              , "distribution given the number of leaves, the birth and the death rate."]

argsParser :: Parser Args
argsParser = Args
  <$> nTreeOpt
  <*> nLeavesOpt
  <*> treeHeightOpt
  <*> lambdaOpt
  <*> muOpt
  <*> sumStatOpt
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

nLeavesOpt :: Parser Int
nLeavesOpt = option auto
  ( long "nLeaves"
    <> short 'n'
    <> metavar "INT"
    <> value 5
    <> showDefault
    <> help "Number of leaves per tree" )

treeHeightOpt :: Parser (Maybe Double)
treeHeightOpt = optional $ option auto
  ( long "height"
    <> short 'H'
    <> metavar "DOUBLE"
    <> help "Fix tree height (no default)" )


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

sumStatOpt :: Parser Bool
sumStatOpt = switch
  ( long "summary-statistics"
    <> short 's'
    <> showDefault
    <> help "Only output number of children for each branch" )

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
  let v = verbosity args
      q = quiet args
      s = sumStat args
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
  res <- if s
         then simulateNBranchLengthNChildrenConcurrently c args
         else simulateNTreesConcurrently c args
  T.putStr res

simulateNTreesConcurrently :: Int -> Args -> IO T.Text
simulateNTreesConcurrently c (Args t n h l m _ v _) = do
  trs <- replicateConcurrently c (simulateNTrees (t `div` c) n h l m v)
  -- If the total number of trees is not divisible by the number of
  -- capabilities, we have to create some more trees.
  trsRe <- simulateNTrees (t `mod` c) n h l m v
  return $ T.unlines $ map toNewickInt (concat trs ++ trsRe)

simulateNTrees :: Int -> Int -> Maybe Double -> Double -> Double -> Bool -> IO [PhyloTree Int]
simulateNTrees t n mH l m v = do
  when v reportCapability
  g <- createSystemRandom
  let f = case mH of
            Nothing -> simulateReconstructedTreeRandomHeight n l m g
            Just h -> simulateReconstructedTree n h l m g
  replicateM t f

simulateNBranchLengthNChildrenConcurrently :: Int -> Args -> IO T.Text
simulateNBranchLengthNChildrenConcurrently c (Args t n h l m _ v _) = do
  trs <- replicateConcurrently c (simulateNBranchLengthNChildren (t `div` c) n h l m v)
  -- If the total number of trees is not divisible by the number of
  -- capabilities, we have to create some more trees.
  trsRe <- simulateNBranchLengthNChildren (t `mod` c) n h l m v
  -- TODO: Format output in a better way.
  -- let treeDats = concat trs ++ trsRe
  --     treeDatToStr td = unlines $ map (show fst ++ show snd) td
  -- return T.pack $ map treeDatToStr treeDats
  return $ T.unlines $ map (T.pack . show) (concat trs ++ trsRe)


simulateNBranchLengthNChildren :: Int -> Int -> Maybe Double -> Double -> Double -> Bool
                               -> IO [[(Double, Int)]]
simulateNBranchLengthNChildren t n mH l m v = do
  when v reportCapability
  g <- createSystemRandom
  let f = case mH of
            Nothing -> simulateBranchLengthNChildrenRandomHeight n l m g
            Just h  -> simulateBranchLengthNChildren n h l m g
  replicateM t f

reportCapability :: IO ()
reportCapability = do
  i <- myThreadId
  (c, _) <- threadCapability i
  putStrLn $ "Running on core: " ++ show c
