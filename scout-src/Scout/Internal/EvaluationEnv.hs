{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.EvaluationEnv where

import Generic.Data

import Optics

import Scout.Node

data EvaluationEnv
    = EvaluationEnv { valueEnv :: Map Identifier (ScopedEvaluation Expr)
                      -- used for tracking all bindings up to the first application (renameShadowedVariables takes over from there)
                    , shadowingEnv :: ShadowingEnv
                    }
    deriving ( Eq, Show, Generic )
    deriving ( Semigroup, Monoid ) via (Generically EvaluationEnv)

makeFieldLabelsNoPrefix ''EvaluationEnv
