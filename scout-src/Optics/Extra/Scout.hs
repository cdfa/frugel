{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Optics.Extra.Scout
    ( module Optics.Extra.Scout
    , module Optics.Extra.Frugel
    , module Optics.ReadOnly.Intro
    , module Optics.ReadOnly.VL
    , module Optics.ReadOnly.FunctorOptic
    ) where

import Data.Has

import GHC.Exts

import Optics.Extra.Frugel    hiding ( foldVL )
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

-- _NonEmpty :: IsList l => Prism' l (NonEmpty (Item l))
-- _NonEmpty = prism' fromFoldable (nonEmpty . toList)
_UnNonEmpty :: IsList l => Lens' (NonEmpty (Item l)) l
_UnNonEmpty = lens fromFoldable (\s b -> fromMaybe s . nonEmpty $ toList b)

afailing' :: ( Is k An_AffineTraversal
             , Is k A_Traversal
             , Is l An_AffineTraversal
             , Is l A_Traversal
             )
    => Optic k is s s a b
    -> Optic l js s s a b
    -> AffineTraversal s s a b
afailing' firstOptic secondOptic
    = atraversal
        (either (matching secondOptic) Right . matching firstOptic)
        (\s b -> fromMaybe s
         $ failover firstOptic (const b) s <|> failover secondOptic (const b) s)

infixl 3 `afailing'`

cosmosOf :: forall k a. Is k A_Fold => Optic' k NoIx a a -> Fold a a
cosmosOf l = simple `summing` castOptic @A_Fold l % cosmosOf l

like :: a -> Getter b a
like a = to (const a)
