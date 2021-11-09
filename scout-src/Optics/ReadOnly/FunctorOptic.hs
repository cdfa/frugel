{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Optics.ReadOnly.FunctorOptic where

import Data.Composition
import Data.Sequence.Optics

import Optics
import Optics.ReadOnly.Intro

class Applicative (ViewFunctor k) => FunctorOptic k where
    type ViewFunctor k :: Type -> Type
    gview' :: MonadReader s m => Optic' k is s r -> m (ViewFunctor k r)
    default gview' :: ( ViewResult k r ~ ViewFunctor k r
                      , ViewableOptic k r
                      , MonadReader s m
                      )
        => Optic' k is s r
        -> m (ViewFunctor k r)
    gview' = gview
    intro' :: (a -> ViewFunctor k r) -> Optic' k NoIx a r

instance FunctorOptic A_Getter where
    type ViewFunctor A_Getter = Identity
    gview' = flip gviews Identity
    intro' f = intro $ runIdentity . f

instance FunctorOptic An_AffineFold where
    type ViewFunctor An_AffineFold = Maybe
    intro' f = to f % _Just

instance FunctorOptic A_Fold where
    type ViewFunctor A_Fold = Seq
    gview' = asks . seqOf
    intro' f = to f % folded

onOptic :: forall k is js ks s a p q o n.
    (JoinKinds k A_Getter k, FunctorOptic k, AppendIndices is js ks)
    => ((s -> ViewFunctor k a)
        -> (p -> ViewFunctor k q)
        -> o
        -> ViewFunctor k n)
    -> Optic' k is s a
    -> Optic' k js p q
    -> Optic' k NoIx o n
onOptic combinator leftOptic rightOptic
    = const' (intro' $ combinator (gview' leftOptic) (gview' rightOptic))
             (leftOptic % magic)
  where
    const' = const -- to avoid hlint hint
    magic
        = error "Use % to remove redundant constraint warning about useful JoinKinds"
            :: Optic' A_Getter js a ()

infixr 3 `fanout`

fanout :: forall m ks k is s a l js q o ls.
    ( Is k o
    , Is l o
    , JoinKinds k l m
    , JoinKinds m A_Getter o
    , JoinKinds o A_Getter o
    , AppendIndices is js ks
    , AppendIndices ks ks ls
    , FunctorOptic o
    )
    => Optic' k is s a
    -> Optic' l js s q
    -> Optic' o NoIx s (a, q)
fanout leftOptic rightOptic
    = const' (onOptic (uncurry (liftA2 (,)) .:. (&&&))
                      (castOptic leftOptic)
                      (castOptic rightOptic))
             (leftOptic % magic rightOptic % magicReversePrism)
  where
    const' = const -- to avoid hlint hint
    magic
        = error "Use % to remove redundant constraint warning about useful JoinKinds k l m"
            :: Optic' l js s q -> Optic' l js a q
    magicReversePrism
        = error "Use % to remove redundant constraint warning about useful JoinKinds m A_Getter o"
            :: Optic' A_Getter ks q f
