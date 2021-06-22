{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Program where

import Control.ValidEnumerable

import Data.Data
import Data.GenValidity
import Data.Has
import Data.Text.Optics

import Frugel

import Optics.Extra

import Scout.Internal.Meta ( ProgramMeta(standardMeta) )
import Scout.Node
import qualified Scout.Parsing as Parsing
import Scout.Parsing     hiding ( expr, whereClause )

import Test.QuickCheck.Gen

import Text.Megaparsec   as Megaparsec hiding ( ParseError, parseErrorPretty )

data Program
    = Program { meta        :: ProgramMeta
              , expr        :: Expr
              , whereClause :: Maybe WhereClause
              }
    | ProgramCstrSite ProgramMeta CstrSite
    deriving ( Show, Eq, Generic, Has ProgramMeta, Data )

type instance NodeOf Program = Node

makeFieldLabelsWith noPrefixFieldLabels ''Program

makePrisms ''Program

instance Has Meta Program where
    getter p = standardMeta $ getter p
    modifier = over (programMeta % #standardMeta)

program' :: Expr -> Maybe WhereClause -> Program
program' = Program $ defaultProgramMeta 1

programMeta :: Lens' Program ProgramMeta
programMeta = hasLens

programCstrSite' :: CstrSite -> Program
programCstrSite' = ProgramCstrSite $ defaultProgramMeta 0

instance CstrSiteNode Program where
    setCstrSite = const . programCstrSite'
    _NodeCstrSite = _ProgramCstrSite % _2

deriving instance Eq (InternalError Program)

deriving instance Eq (Error Program)

deriving instance Eq (Model Program)

deriving instance Show (InternalError Program)

deriving instance Show (Error Program)

deriving instance Show (Model Program)

instance Editable Program

instance DisplayProjection (Error Program) where
    renderDoc = \case
        ParseError e -> parseErrorPretty e
        InternalError e -> "Internal error:" <+> renderDoc e

instance Parseable Program where
    type ParserOf Program = Parser
    type ParseErrorOf Program = ParseError
    programParser
        = setProgramWhitespace
        <$> (program' <$%> Parsing.expr <*%> optional Parsing.whereClause
             <*% pure ())
      where
        setProgramWhitespace :: WithWhitespace Program -> Program
        setProgramWhitespace
            ( trailingWhitespace : whitespaceFragments -- whitespace fragments are reversed
            , p
            )
            = set (#meta % #trailingWhitespace) trailingWhitespace
            $ setWhitespace (whitespaceFragments, p)
        setProgramWhitespace _ = error "not enough whitespace fragments"
    anyNodeParser = anyNode
    runParser parser cstrSite
        = first (fmap (fixErrorOffset @Program cstrSite) . bundleErrors)
        $ Megaparsec.runParser (parser <* eof) "document" cstrSite
    errorOffset = Parsing.errorOffset

instance Decomposable Program where
    mapMComponents mapChar mapNode program@Program{}
        = chain
            (intersperseWhitespaceTraversals
                 mapChar
                 program
                 [ traverseOf #expr mapNode, #whereClause % _Just %%~ mapNode ]
             :> (#meta % #trailingWhitespace % unpacked % traversed
                 %%~ mapChar))
            program
    mapMComponents mapChar mapNode (ProgramCstrSite meta materials)
        = ProgramCstrSite meta <$> mapMComponents mapChar mapNode materials

instance DisplayProjection Program

instance AnnotatedPretty Program where
    annPretty (ProgramCstrSite _ contents) = annPretty contents
    -- Weird definition to avoid shadowing
    annPretty program = annPretty . expr <> annPretty . whereClause $ program

instance Validity Program where
    validate
        = mconcat [ genericValidate
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
        = datatype [ addMetaWith enumerateValidProgramMeta (uncurry . Program)
                   , addMetaWith enumerateValidProgramMeta ProgramCstrSite
                   ]
