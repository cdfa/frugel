{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Optics.Extra.Scout
    ( module Optics.Writer
    , module Optics.Extra.Scout
    , module Optics.Extra.Frugel
    ) where

import Data.Has

import GHC.Exts

import Optics.At.Orphans ()
import Optics.Extra.Frugel
import Optics.Writer

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

_NonEmpty :: (IsList l, Item l ~ a) => Prism' l (NonEmpty a)
_NonEmpty = prism' fromFoldable (nonEmpty . toList)

infixr 4 <<.~, <<%~

(<<%~) :: PermeableOptic k a
    => Optic k is s t a b
    -> (a -> b)
    -> s
    -> (ViewResult k a, t)
o <<%~ f = passthrough o $ \a -> (a, f a)

(<<.~) :: PermeableOptic k a
    => Optic k is s t a b
    -> b
    -> s
    -> (ViewResult k a, t)
o <<.~ b = o <<%~ const b