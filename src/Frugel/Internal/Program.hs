{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Program where

import           Data.GenValidity
import           Data.Has

import           Frugel.Internal.Meta ( ProgramMeta(standardMeta) )
import           Frugel.Node

import           Optics

data Program
    = Program { meta        :: ProgramMeta
              , expr        :: Expr
              , whereClause :: Maybe WhereClause
              }
    | ProgramCstrSite ProgramMeta CstrSite
    deriving ( Show, Eq, Generic, Has ProgramMeta )

makeFieldLabelsWith noPrefixFieldLabels ''Program

instance Has Meta Program where
    getter p = standardMeta $ getter p
    modifier = over (programMeta % #standardMeta)

programMeta :: Lens' Program ProgramMeta
programMeta = hasLens

programCstrSite' :: CstrSite -> Program
programCstrSite' = ProgramCstrSite $ defaultProgramMeta 0

instance Default (Getter CstrSite Program) where
    def = to programCstrSite'

instance Validity Program where
    validate = mconcat [ genericValidate, validateInterstitialWhitespace $ \case
        Program{} -> 1
        ProgramCstrSite{} -> 0 ]
