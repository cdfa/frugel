{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Program where

import           Control.ValidEnumerable

import           Data.Data
import           Data.GenValidity
import           Data.Has

import           Frugel.Internal.Meta    ( ProgramMeta(standardMeta) )
import           Frugel.Node

import           Optics

import           Test.QuickCheck.Gen

data Program
    = Program { meta        :: ProgramMeta
              , expr        :: Expr
              , whereClause :: Maybe WhereClause
              }
    | ProgramCstrSite ProgramMeta CstrSite
    deriving ( Show, Eq, Generic, Has ProgramMeta, Data )

makeFieldLabelsWith noPrefixFieldLabels ''Program

makePrisms ''Program

instance Has Meta Program where
    getter p = standardMeta $ getter p
    modifier = over (programMeta % #standardMeta)

programMeta :: Lens' Program ProgramMeta
programMeta = hasLens

programCstrSite' :: CstrSite -> Program
programCstrSite' = ProgramCstrSite $ defaultProgramMeta 0

instance SetCstrSite Program where
    setCstrSite = const . programCstrSite'

instance Validity Program where
    validate
        = mconcat
            [ genericValidate
            , validateInterstitialWhitespace validInterstitialWhitespace
            ]

instance ValidInterstitialWhitespace Program where
    validInterstitialWhitespace = \case
        Program{} -> 1
        ProgramCstrSite{} -> 0

instance GenValid Program where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance ValidEnumerable Program where
    enumerateValid
        = datatype
            [ addMetaWith enumerateValidProgramMeta (uncurry . Program)
            , addMetaWith enumerateValidProgramMeta ProgramCstrSite
            ]
