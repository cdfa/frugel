{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Fallible where

import Data.Type.Equality

import Optics
import Optics.Internal.Utils

class FallibleOptic k l m s t a b | k l -> m where
    {-|
    NB: @gfailing k l@ only produces legal R/W optics when the foci of @k@ and @l@ are disjoint or @l@ focuses at least as "deep" as @k@.
    For example, @_tail \`gfailing\` simple@ is illegal, because composition is not preserved, i.e. with @o = _tail \`gfailing\` simple@, @Identity . cons 1@ and @g = Identity . cons 2@, we have
    
    @
    getCompose $ traverseOf o (Compose . fmap f . g) [] ≡ Identity (Identity [ 1 , 2 ])
        /= Identity (Identity [ 2 , 1 ]) ≡ fmap (traverseOf o f) $ traverseOf o g []
    @

    On the other hand, @ix i \`gfailing\` _last@ would be legal.
    In other words, the success/failure of @k@ cannot depend on the values of @l@'s foci.
    -}
    gfailing
        :: Optic k is s t a b -> Optic l is s t a b -> Optic m NoIx s t a b

infixl 3 `gfailing` -- Same as (<|>)

-- Prism
instance FallibleOptic A_Prism An_Iso A_Lens s t a b where
    gfailing k l = gfailing k $ castOptic @A_Lens l

instance FallibleOptic A_Prism A_Lens A_Lens s t a b where
    gfailing k l = gfailing (castOptic @An_AffineTraversal k) l

instance FallibleOptic A_Prism A_Prism An_AffineTraversal s t a b where
    gfailing = gfailing `on` castOptic @An_AffineTraversal

instance FallibleOptic A_Prism A_Getter A_Getter s s a a where
    gfailing k l = gfailing (castOptic @An_AffineFold k) l

-- AffineTraversal
instance FallibleOptic An_AffineTraversal A_Lens A_Lens s t a b where
    gfailing k l = withAffineTraversal k $ \matchK _ -> withLens l $ \viewL _ ->
        lens (\s -> fromRight (viewL s) $ matchK s)
             (\s b -> fromMaybe (set l b s) $ failover k (const b) s)

instance FallibleOptic An_AffineTraversal An_AffineTraversal An_AffineTraversal s t a b where
    gfailing k l = atraversalVL $ \point f s -> let
        OrT visited fu = atraverseOf k (OrT False . point) (wrapOrT . f) s
        in if visited then fu else atraverseOf l point f s

-- AffineFold
instance FallibleOptic An_AffineFold A_Getter A_Getter s s a a where
    gfailing k l = to $ \s -> fromMaybe (view l s) $ preview k s

instance FallibleOptic An_AffineFold An_AffineFold An_AffineFold s s a a where
    gfailing k l = afolding $ \s -> preview l s <|> preview k s

-- Traversal
instance FallibleOptic A_Traversal A_Traversal A_Traversal s t a b where
    gfailing k l = traversalVL $ \f s -> let
        OrT visited fu = traverseOf k (wrapOrT . f) s
        in if visited then fu else traverseOf l f s

-- Fold
instance FallibleOptic A_Fold A_Fold A_Fold s s a a where
    gfailing k l = foldVL $ \f s -> let
        OrT visited fu = traverseOf_ k (wrapOrT . f) s
        in if visited then fu else traverseOf_ l f s

instance {-# OVERLAPPABLE #-}( JoinKinds k l m
                             , Is k m
                             , Is l m
                             , FallibleOptic m m m s t a b
                             , (k == l) ~ 'False
                             ) => FallibleOptic k l m s t a b where
    gfailing k l = gfailing (castOptic @m k) (castOptic @m l)
