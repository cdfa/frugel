{-# LANGUAGE RankNTypes #-}

module Control.ValidEnumerable.Access ( uniformValid, uniformWith ) where

import           Control.ValidEnumerable.Class

import           Data.ClassSharing

import           Prelude                       hiding ( local )

import           Test.Feat
import qualified Test.Feat.Access              as Feat
import           Test.QuickCheck.Gen

uniformValid :: ValidEnumerable a => Int -> Gen a
uniformValid = uniformWith accessValid

uniformWith :: Shareable Enumerate a -> Int -> Gen a
uniformWith = Feat.uniformWith . global

{-# NOINLINE gref #-}
gref :: Ref
gref = unsafeNewRef ()

global :: Shareable f a -> f a
global access = run access gref
-- | Guarantees local sharing. All enumerations are shared inside each invocation of local, but may not be shared between them.
-- {-# INLINE local #-}
-- local :: Shareable f a -> f a
-- local access = run access (unsafeNewRef ())
