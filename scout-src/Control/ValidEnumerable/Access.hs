{-# LANGUAGE RankNTypes #-}

module Control.ValidEnumerable.Access
    ( uniformValid
    , uniformWith
    , values
    , valuesWith
    ) where

import Control.ValidEnumerable.Class

import Data.ClassSharing

import Prelude                 hiding ( local )

import Test.Feat               hiding ( values )
import qualified Test.Feat.Access as Feat
import Test.QuickCheck.Gen

uniformValid :: ValidEnumerable a => Int -> Gen a
uniformValid = uniformWith accessValid

uniformWith :: Shareable Enumerate a -> Int -> Gen a
uniformWith = Feat.uniformWith . global

values :: ValidEnumerable a => [(Integer, [a])]
values = valuesWith accessValid

-- | Non class version of 'values'.
valuesWith :: Shareable Enumerate a -> [(Integer, [a])]
valuesWith = Feat.valuesWith . global

{-# NOINLINE gref #-}
gref :: Ref
gref = unsafeNewRef ()

global :: Shareable f a -> f a
global access = run access gref
-- | Guarantees local sharing. All enumerations are shared inside each invocation of local, but may not be shared between them.
-- {-# INLINE local #-}
-- local :: Shareable f a -> f a
-- local access = run access (unsafeNewRef ())
