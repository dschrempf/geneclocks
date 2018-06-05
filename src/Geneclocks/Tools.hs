{- |
   Modules     :  Geneclocks.Tools
   Description :  Tools
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Mon Feb 12 18:25:26 2018.

Some utility functions.

-}

module Geneclocks.Tools
  ( realFloatToText
  , integralToText
  , minTuple
  , minimumWithIndex
  , sortWithIndices
  , minimumsIndices
  , fstOfThree
  , sndOfThree
  , trdOfThree
  , showRoundedFloatPrec
  , showRoundedFloat
  , realFloatBuilder
  )
  where

import           Data.Function
import           Data.List
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as T (toStrict)
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import           Numeric

-- | Convert a float value to a text object.
realFloatToText :: RealFloat a => a -> T.Text
realFloatToText = T.toStrict . B.toLazyText . B.realFloat

-- | Convert an integral value to a text object.
integralToText :: Integral a => a -> T.Text
integralToText = T.toStrict . B.toLazyText . B.decimal

-- | Minimum of 2-Tuple.
minTuple :: Ord a => (a, a) -> a
minTuple (l, r) = if l<r then l else r

-- | Get minimum of list and its index.
minimumWithIndex :: Ord a => [a] -> (a, Int)
minimumWithIndex xs = minimum $ zip xs ([0..] :: [Int])

-- | Sort a list and also return original indices.
sortWithIndices :: Ord a => [a] -> [(a, Int)]
sortWithIndices xs = sortBy (compare `on` fst) $ zip xs ([0..] :: [Int])

-- | Minimum and second minimum of a list with their indices.
minimumsIndices :: Ord a => [a] -> [(a, Int)]
minimumsIndices xs = take 2 $ sortWithIndices xs

-- | The first element of a three-Tuple.
fstOfThree :: (a, b, c) -> a
fstOfThree (x, _, _) = x

-- | The second element of a three-Tuple.
sndOfThree :: (a, b, c) -> b
sndOfThree (_, x, _) = x

-- | The third element of a three-Tuple.
trdOfThree :: (a, b, c) -> c
trdOfThree (_, _, x) = x

-- | Show a real float with precision 'n'.
showRoundedFloatPrec :: RealFloat a => Int -> a -> String
showRoundedFloatPrec n d = showFFloat (Just n) d ""

precision :: Maybe Int
precision = Just 6

-- | Show a real float with globally defined 'precision'.
showRoundedFloat :: RealFloat a => a -> String
showRoundedFloat d = showFFloat precision d ""

-- | Text builder of real float with globally defined 'precision'.
realFloatBuilder :: RealFloat a => a -> B.Builder
realFloatBuilder = B.formatRealFloat B.Generic precision
