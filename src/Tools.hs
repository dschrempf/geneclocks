{- |
   Description :  Tools
   Copyright   :  (c) Dominik Schrempf 2018
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Mon Feb 12 18:25:26 2018.

Some utility functions.

-}

module Tools
  ( realFloatToText
  , intToText
  )
  where

import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

-- | Convert a float value to a text object.
realFloatToText :: RealFloat a => a -> T.Text
realFloatToText = T.toStrict . B.toLazyText . B.realFloat

-- | Convert an integral value to a text object.
intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal
