{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Optics.Extra.Scout
    ( module Optics.Extra.Scout
    , module Optics.Extra.Frugel
    , module Optics.ReadOnly.Intro
    , module Optics.ReadOnly.VL
    , module Optics.ReadOnly.FunctorOptic
    , module Optics.Fallible
    ) where

import Data.Has

import Optics.Extra.Frugel    hiding ( foldVL )
import Optics.Fallible
import Optics.ReadOnly.FunctorOptic
import Optics.ReadOnly.Intro
import Optics.ReadOnly.VL

-- Can't use tuple directly, because GHC can't do impredicative types yet
-- data instead of newtype because of existential quantification
data Traverser' f k is s = forall a. Traverser' (Optic' k is s a) (a -> f a)

newtype Disjoint a = Disjoint { unDisjoint :: [a] }

chainDisJoint :: (Applicative f, Is k A_Setter, Is k An_AffineFold)
    => n
    -> Disjoint (Traverser' f k is n)
    -> f n
chainDisJoint s = foldr foldOp (pure s) . unDisjoint
  where
    foldOp (Traverser' optic f) s' = maybe s' setComponent $ preview optic s
      where
        setComponent component = set optic <$> f component <*> s'

concatByPrism :: (Is k An_AffineFold, Is k A_Review, Monoid a)
    => Optic' k is s a
    -> [s]
    -> [s]
concatByPrism p = concatBy (preview p) (review p)

hasLens :: Has a s => Lens' s a
hasLens = lens getter (\t b -> modifier (const b) t)

fromAffineFold :: Is k An_AffineFold => a -> Optic' k is s a -> s -> a
fromAffineFold a afold = fromMaybe a . preview afold

cosmosOf :: forall k a. Is k A_Fold => Optic' k NoIx a a -> Fold a a
cosmosOf l = simple `summing` castOptic @A_Fold l % cosmosOf l

like :: a -> Getter b a
like a = to (const a)
