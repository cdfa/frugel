{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Program where

import           Data.Has

import           Frugel.Decomposition
import           Frugel.Internal.Meta
                 ( Meta(interstitialWhitespace), ProgramMeta(standardMeta) )
import           Frugel.Node          hiding ( whereClause )

import           Optics

data Program
    = Program { meta :: ProgramMeta, expr :: Expr, whereClause :: WhereClause }
    | ProgramCstrSite ProgramMeta CstrMaterials
    deriving ( Show, Eq, Generic, Has ProgramMeta )

makeFieldLabelsWith noPrefixFieldLabels ''Program

instance Has Meta Program where
    getter p = standardMeta $ getter p
    modifier = over (programMeta % #standardMeta)

instance Decomposable Program where
    decomposed Program{..}
        = decomposed
        . intersperseWhitespace (interstitialWhitespace $ standardMeta meta)
        $ fromList [ Right $ ExprNode expr, Right $ WhereNode whereClause ]
    decomposed (ProgramCstrSite _ materials) = decomposed materials

programMeta :: Lens' Program ProgramMeta
programMeta = hasLens
