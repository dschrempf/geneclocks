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
  ( ILabel(..)
  , IState(..)
  , INodeType(..)
  , coalescentString
  , ITree
  , iStateFromInts
  , iAgree
  ) where

-- import           Control.DeepSeq
import           Data.Maybe
import qualified Data.Set                  as S
import           Geneclocks.Tools
import           Geneclocks.Tree.Phylo
import           Geneclocks.Tree.Species
import           GHC.Generics              (Generic)

-- | Individual name.
newtype ILabel a = ILabel a deriving (Eq, Ord, Read, Show, Generic, Enum, Num, Real, Integral)

-- | State of individual.
newtype IState a = IState (ILabel a, SLabel a) deriving (Eq, Ord, Read, Generic)

instance Show a => Show (IState a) where
  show (IState (ILabel i, s)) = wrap 'I' ++ show i ++ show s

-- | A helper function to create 'iState's from Int labels.
iStateFromInts :: Int -> Int -> IState Int
iStateFromInts i s = IState (ILabel i, SLabel s)

iStateToILabel :: IState a -> ILabel a
iStateToILabel (IState (l, _)) = l

iStateToSLabel :: IState a -> SLabel a
iStateToSLabel (IState (_, l)) = l

-- | Node types for individuals (on species).

-- XXX: Before calculation of the likelihood for substitution models, all degree
-- two nodes have to be removed anyways... complicated.

-- Should we just ignore degree two nodes? But then, the notion that each gene
-- can be assigned to an individual and a species is difficult.

data INodeType =
  -- | By definition a degree two node. The underlying species merges, and the
  -- 'SLabel' changes to the one of the ancestor (going backwards).
  ISCoalescence
  -- | Individuals coalesce. A coalescence of individuals changes the 'ILabel'.
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

-- | Check if an individual tree is valid.
--
-- Performed checks:
--
--   - Validity of individual tree.
--
--   - Validity of species tree.
--
--   - Backwards in time: no coalescence of any pair of individuals before the
--     respective species coalesce.
iAgree :: (Show a, Eq a, Ord a, Show b, Ord b, Num b) => ITree a b -> STree a b -> Bool
iAgree i s = valid i &&
             valid s &&
             iCheckHeightsNSplits s (heightsNSplits i)

-- | Check if the species tree agrees with the [(Height, Split)] list.
iCheckHeightsNSplits :: (Show a, Ord a, Show b, Ord b, Num b) => STree a b -> [(b, ITree a b)] -> Bool
iCheckHeightsNSplits s = all (iCheckHeightNSplit s)

-- TODO: Even if the height of a speciation matches, a subsequent coalescence of
-- individuals (backwards in time) has to have the same branch length. This is
-- not yet checked here. All this gets very complicated...

-- Nomenclature: (S)pecies, (I)ndividual, (N)ode, (T)ype, (Tr)ree, (D)aughter
iCheckHeightNSplit :: (Show a, Ord a, Show b, Ord b, Num b)
                   => STree a b -> (b, ITree a b) -> Bool
iCheckHeightNSplit s (h, i)
  | iNT == ISCoalescence =
      -- Heights of speciations agree.
      h  == sH &&
      -- Ancestors and daughter species agree.
      rootNodesAgree iStateToSLabel id i sMrcaTr &&
      -- Force degree two node.
      degree i == 2
  | iNT == ICoalescence  =
      -- Individuals need to be in the same species.
      isSingleton iDSs &&
      -- Ancestor and daughter individuals need to be different.
      isPairwiseDistinct (iL : iDIs)
  | external iNT        = iS `elem` map label (getLeaves s)
  | otherwise           = error "INodeType did not pattern match. Weird."
  where
    iN = rootNode i
    iS = iStateToSLabel . label $ iN
    iNT = nodeType iN
    iL = iStateToILabel . label $ iN
    iDs = subForest i
    iDSs = S.fromList $ map (iStateToSLabel . label . rootNode) iDs
    iDIs = map (iStateToILabel . label . rootNode) iDs
    (sH, sMrcaTr) = fromMaybe (error $ "MRCA of " ++ show iDSs ++ " not present in species tree " ++ show s)
                (mrcaHeightNTree (S.singleton iS) s)
