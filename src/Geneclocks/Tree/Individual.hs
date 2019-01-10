-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      :  Geneclocks.Tree.Individual
   Description :  Individual trees
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Evolution of individuals in species. A leaf is an individual that belongs to a a
species. Since we do not have data for different individuals, there will be only
one individual per species at the leaves of the tree. However, this notion of an
individual belonging to a species is important when species merge going
backwards in time. Because then, individuals from different species will be
present in the same ancestral species.


TODO: What can be done to avoid re-computation of heights, leaf sets, and so on?
-}

module Geneclocks.Tree.Individual
  ( IName(..)
  , IState(..)
  , iStateFromInts
  , iStateToIName
  , iStateToSName
  , INodeType(..)
  , coalescenceString
  , ITree

  , iRootNodeIName
  , iRootNodeSName
  , assertISAgreement
  ) where

-- import           Control.DeepSeq
import           Control.Error
import           Data.Foldable
import qualified Data.Set                as S
import           Geneclocks.Tools
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics            (Generic)

-- | Individual name.
newtype IName a = IName {iName :: a} deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

-- | State of individual.
newtype IState a = IState {iState :: (IName a, SName a)} deriving (Eq, Ord, Read, Generic)
instance Show a => Show (IState a) where
  -- show (IState (IName i, s)) = wrap 'I' ++ show i ++ show s
  show (IState (IName i, s)) = 'I' : show i ++ show s

-- | A helper function to create 'iState's from Int names.
iStateFromInts :: Int -> Int -> IState Int
iStateFromInts i s = IState (IName i, SName s)

-- | Extract 'IName' from 'IState'.
iStateToIName :: IState a -> IName a
iStateToIName = fst . iState

-- | Extract 'SName' from 'IState'.
iStateToSName :: IState a -> SName a
iStateToSName  = snd . iState

-- | Node types for individuals (on species).
data INodeType =
  -- | By definition a degree two node. The underlying species merges, and the
  -- 'SName' changes to the one of the ancestor (going backwards).
  ISCoalescent
  -- | Individuals coalesce. A coalescence of individuals changes the 'IName'.
  -- This event involves at least two daughter lineages.
  | ICoalescent
  | IExtant  -- ^ Extant leaf.
  | IExtinct -- ^ Extinct leaf.
  deriving (Eq, Read, Generic)

-- | Denote coalescent.
coalescenceString :: String
coalescenceString = wrap 'C'

instance Show INodeType where
  show ISCoalescent = speciationString
  show ICoalescent  = coalescenceString
  show IExtant      = existenceString
  show IExtinct     = extinctionString

instance NodeType INodeType where
  extant  IExtant = True
  extant  _       = False
  extinct IExtinct = True
  extinct _        = False
  defaultExternal  = IExtant
  defaultInternal  = ICoalescent

-- | A gene individual tree.
type ITree a b = PhyloTree (IState a) b INodeType

-- | Get the individual name of the root node state.
iRootNodeIName :: ITree a b -> IName a
iRootNodeIName = iStateToIName . rootNodeState

-- | Get the species name of the root node state.
iRootNodeSName :: ITree a b-> SName a
iRootNodeSName = iStateToSName . rootNodeState


-- | Check if an individual tree agrees with a species tree.
--
-- Performed checks:
--   - Validity, clock-likeness of both trees.
--   - Heights have to be equal.
--   - No coalescence of any pair of individuals later than the coalescence of
--     the respective species.
--   - For a speciation:
--     - Heights of speciations have to agree.
--     - Ancesotrs and daughter species have to agree.
--     - Node has to have degree two.
--   - For a coalescence:
--     - Species names cannot change and have to be equal.
--     - Ancestor and daughter individuals need to be different.
--   - For external nodes:
--     - The species name has to be present in the leaves of the species tree.
--
-- TODO: Even if the height of a speciation matches, a subsequent coalescence of
-- individuals (backwards in time) has to have the same branch length. This is
-- not yet checked here. All this gets very complicated...
--
-- I am still unsure whether this function is fail proof. I.e., it may still
-- give a TRUE for some fancy bullshit 'ITree' that actually does not agree with
-- the 'STree'...

assertISAgreement :: (Show a, Eq a, Ord a, ApproxEq b, Show b, Ord b, Num b) => ITree a b -> STree a b -> Either String ()
assertISAgreement i s =
  assertErr "Individual tree is not valid." (valid i) >>
  assertErr "Individual tree is not clock-like." (clockLike i) >>
  assertErr "Species tree is not valid." (valid s) >>
  assertErr "Species tree is not clock-like." (clockLike s) >>
  assertErr "Extinction have to happen before present." (all (<= h) dXLs) >>
  assertErr "Heights of individual and species tree are not equal." (h == heightClockLike s) >>
  assertErr "Individual and species root node states do not match." (iRootNodeSName i == rootNodeState s) >>
  assertErr "Some extant species of individual tree not present in species tree." (iSs `S.isSubsetOf` sSs) >>
  assertIHeightsNSplits s (heightsNSplits i)
  where iSs  = S.fromList $ map (iStateToSName . state) (getExtantLeaves i)
        sSs  = S.fromList $ map state (getExtantLeaves s)
        dXLs = distancesOriginExtinctLeaves i
        h    = heightClockLike i

-- Check if the species tree agrees with the [(Height, Split)] list.
assertIHeightsNSplits :: (Show a, Ord a, ApproxEq b, Show b, Ord b, Num b) => STree a b -> [(b, ITree a b)] -> Either String ()
assertIHeightsNSplits s = traverse_ (assertIHeightNSplit s)

-- Check the speciation at the root of the tree.
iSpeciationAgrees :: (Ord a, Eq b) => ITree a b -> STree a b -> Bool
iSpeciationAgrees i s = rootNodeType i == ISCoalescent &&
                        rootNodeType s == SCoalescent &&
                        rootNodesAgreeWith (iStateToSName . state) state i s

-- Nomenclature: (S)pecies, (I)ndividual, (N)ode, (T)ype, (Tr)ree, (D)aughter
assertIHeightNSplit :: (Show a, Ord a, Show b, ApproxEq b, Ord b, Num b)
                   => STree a b -> (b, ITree a b) -> Either String ()
assertIHeightNSplit s (h, i)
  | iNT == ISCoalescent =
      -- Heights of speciations are equal.
      assertErr "Heights of a speciation do not match between individual and species tree." (h  =~= sH) >>
      -- Ancestors and daughter species agree.
      assertErr "A speciation does not agree between individual and species tree " (iSpeciationAgrees i sMrcaTr) >>
      -- Force degree two node.
      assertErr "The degree of a speciation node of the individual tree is not two." (rootDegree i == 2) >>
      -- Individual name does not change.
      assertErr "The individual name changes at a speciation event." (iRootNodeIName i `elem` map iRootNodeIName iDs)
  | iNT == ICoalescent  =
      -- The ancestor and daughter species have to be equal.
      assertErr "Ancestor and daughter species have to be equal at a coalescence." (isSingleton (S.insert iS iDSs)) >>
      -- Ancestor and daughter individuals have to be different.
      assertErr "Ancestor and daughter individuals have to be different at a coalescence." (isPairwiseDistinct (iRootNodeIName i : iDIs))
  | extinct iNT = assertErr "Extinct gene in non-existant in individual tree." (isJust mSMrcaTr)
  -- Extant leaves have been checked before.
  | extant iNT  = pure ()
  | otherwise   = error "INodeType did not pattern match. Weird."
  where
    iS            = iRootNodeSName i
    iNT           = rootNodeType i
    iDs           = subForest i
    iDSs          = S.fromList $ map iRootNodeSName iDs
    iDIs          = map iRootNodeIName iDs
    mSMrcaTr      = mrcaHeightNTree (S.singleton iS) s
    (sH, sMrcaTr) = fromMaybe
                    (error $ "MRCA of " ++ show iDSs ++ " not present in species tree " ++ show s)
                    mSMrcaTr
