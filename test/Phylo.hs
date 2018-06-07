module Phylo
  where

import           Control.Monad.Primitive        (PrimMonad)
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Geneclocks.Simulate.Coalescent
import           Geneclocks.Tools
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.PhyloNewick
import           Geneclocks.Tree.Species
import           System.Random.MWC
import           TestIO
import qualified Text.PrettyPrint.ANSI.Leijen   as Pp

-- This should be the following tree
-- ((((0:0.14564913949916541,1:0.14564913949916541)3:0.19181475042041746,2:0.33746388991958287)2:0.2965674875738314,(3:0.3658392682039222,(4:6.66529143516425e-3,5:6.66529143516425e-3)5:0.35917397676875795)4:0.26819210928949205)1:0.7073783929641936,(((6:1.6761313485791404e-2,7:1.6761313485791404e-2)8:2.853071100614925e-2,8:4.5292024491940655e-2)7:0.6385335106273144,9:0.6838255351192551)6:0.6575842353383528)0:0.0;
tree10 :: (PrimMonad m) => m (PhyloTree Int Double SNodeType)
tree10 = do
  g <- create
  t <- simulate 10 g
  return $ numberInternalNodes t

-- We test, if the most recent common ancestors are calculated correctly.
leafSet :: S.Set Int
leafSet = S.fromList [0, 1, 4]

mrcaNode3 :: PrimMonad m => m (Maybe (Info Int Double SNodeType))
mrcaNode3 = mrcaNode leafSet <$> tree10

mrcaTree3 :: PrimMonad m => m (Maybe (PhyloTree Int Double SNodeType))
mrcaTree3 = mrcaTree leafSet <$> tree10

splits :: PrimMonad m => m [(Double, PhyloTree Int Double SNodeType)]
splits = heightsNSplitsOrdered <$> tree10

performTests :: IO ()
performTests = do

  announce "Simulated tree."
  tStr <- T.unpack . toNewickIntegral <$> tree10
  report tStr

  announce $ "Most recent common ancestor of leaf set " ++ (show . S.toList) leafSet ++ "."
  m <- mrcaNode3
  report $ maybe (error "No MRCA.") show m
  if (label <$> m) == Just 1 && ((internal . nodeType <$> m) == Just True)
    then report "Should be 1, success."
    else error "Failure."

  announce "Corresponding subtrees."
  mT <- mrcaTree3
  report $ maybe (error "No MRCA tree.") (T.unpack . toNewickIntegral) mT

  announce "Heights with corresponding splits, ordered."
  ss <- splits
  let ssDoc = Pp.vcat [(Pp.text . showRoundedFloat) h Pp.<>
                        Pp.text ": " Pp.<>
                        (Pp.text . T.unpack . toNewickIntegral $ t)
                      | (h, t) <- ss]
  reportDoc ssDoc
