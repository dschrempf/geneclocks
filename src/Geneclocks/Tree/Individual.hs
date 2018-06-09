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
  , coalescentString
  , ITree
  , iRootNodeIName
  , iRootNodeSName
  , iAgree
  ) where

-- import           Control.DeepSeq
import           Data.Maybe
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
  show (IState (IName i, s)) = wrap 'I' ++ show i ++ show s

-- | A helper function to create 'iState's from Int names.
iStateFromInts :: Int -> Int -> IState Int
iStateFromInts i s = IState (IName i, SName s)

iStateToIName :: IState a -> IName a
iStateToIName = fst . iState

iStateToSName :: IState a -> SName a
iStateToSName  = snd . iState

-- | Node types for individuals (on species).
data INodeType =
  -- | By definition a degree two node. The underlying species merges, and the
  -- 'SName' changes to the one of the ancestor (going backwards).
  ISCoalescence
  -- | Individuals coalesce. A coalescence of individuals changes the 'IName'.
  -- This event involves at least two daughter lineages.
  | ICoalescence
  | IExtant  -- ^ Extant leaf.
  | IExtinct -- ^ Extinct leaf.
  deriving (Eq, Read, Generic)

-- | Denote coalescent.
coalescentString :: String
coalescentString = wrap 'C'

instance Show INodeType where
  show ISCoalescence = speciationString
  show ICoalescence  = coalescentString
  show IExtant       = extantString
  show IExtinct      = extinctString

instance NodeType INodeType where
  extant  IExtant = True
  extant  _       = False
  extinct IExtinct = True
  extinct _        = False
  defaultExternal  = IExtant
  defaultInternal  = ICoalescence

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
--   - Height has to be equal.
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
-- I am still unsure whether this function gives a TRUE for some fancy bullshit
-- 'ITree's that actually do not agree with the 'STree'...

iAgree :: (Show a, Eq a, Ord a, Show b, Ord b, Num b) => ITree a b -> STree a b -> Bool
iAgree i s = valid i && clockLike i &&
             valid s && clockLike s &&
             heightClockLike i == heightClockLike s &&
             iRootNodeSName i == rootNodeState s &&
             iCheckHeightsNSplits s (heightsNSplits i)

-- Check if the species tree agrees with the [(Height, Split)] list.
iCheckHeightsNSplits :: (Show a, Ord a, Show b, Ord b, Num b) => STree a b -> [(b, ITree a b)] -> Bool
iCheckHeightsNSplits s = all (iCheckHeightNSplit s)

-- Nomenclature: (S)pecies, (I)ndividual, (N)ode, (T)ype, (Tr)ree, (D)aughter
iCheckHeightNSplit :: (Show a, Ord a, Show b, Ord b, Num b)
                   => STree a b -> (b, ITree a b) -> Bool
iCheckHeightNSplit s (h, i)
  | iNT == ISCoalescence =
      -- Heights of speciations are equal.
      h  == sH &&
      -- Ancestors and daughter species agree.
      rootNodesAgree iStateToSName id i sMrcaTr &&
      -- Force degree two node.
      rootDegree i == 2
  | iNT == ICoalescence  =
      -- The species cannot change and have to be equal.
      isSingleton (S.insert iS iDSs) &&
      -- Ancestor and daughter individuals need to be different.
      isPairwiseDistinct (iRootNodeIName i : iDIs)
  | external iNT        = iS `elem` map state (getLeaves s)
  | otherwise           = error "INodeType did not pattern match. Weird."
  where
    iS            = iRootNodeSName i
    iNT           = rootNodeType i
    iDs           = subForest i
    iDSs          = S.fromList $ map iRootNodeSName iDs
    iDIs          = map iRootNodeIName iDs
    (sH, sMrcaTr) = fromMaybe
                    (error $ "MRCA of " ++ show iDSs ++ " not present in species tree " ++ show s)
                    (mrcaHeightNTree (S.singleton iS) s)
