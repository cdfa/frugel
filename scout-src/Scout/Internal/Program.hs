{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Scout.Internal.Program where

import qualified Control.Sized
import Control.Sized                     ( aconcat )
import Control.ValidEnumerable
import Control.ValidEnumerable.Whitespace

import Data.Composition
import Data.Data
import Data.GenValidity
import Data.Has
import Data.Sized
import qualified Data.Text               as Text
import Data.Text.Optics

import Frugel
import Frugel.Decomposition
import Frugel.PrettyPrinting

import Optics.Extra

import Prettyprinter.Render.Util.SimpleDocTree

import Scout.Internal.Meta               ( ProgramMeta(standardMeta) )
import Scout.Node
import qualified Scout.Parsing           as Parsing hiding ( node )
import Scout.Parsing                     hiding ( expr, node, whereClause )
import Scout.PrettyPrinting

import Test.QuickCheck.Gen

import Text.Megaparsec
    hiding ( ParseError, parseErrorPretty, runParser )
import qualified Text.Megaparsec         as Megaparsec

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
            ( "" : [trailingWhitespace] -- whitespace fragments are reversed
            , p@Program{whereClause = Nothing}
            )
            = set (#meta % #trailingWhitespace) trailingWhitespace
            $ setWhitespace ([], p)
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

instance PrettyPrint Program where
    prettyPrint program
        = second toList
        . reparse programParser
        . flip setCstrSite program
        . liftNestedCstrSiteOuterWhitespace
        . renderSimplyDecorated (fromList . map Left . toString)
                                renderAnnotation
        . removeRootCstrSiteAnnotation -- remove root construction site annotation, because a ExprNode won't be accepted as a program
        . treeForm
        . layoutSmart defaultLayoutOptions
        $ unsafePrettyProgram program
      where
        removeRootCstrSiteAnnotation
            (STAnn (CompletionAnnotation (InConstruction' _)) subTree)
            | ProgramCstrSite{} <- program = subTree
        removeRootCstrSiteAnnotation ann = ann
        renderAnnotation (CompletionAnnotation (InConstruction' n)) cstrSite
            = one . Right $ setCstrSite cstrSite n
        renderAnnotation _ cstrSite
            = one . Right . ExprNode $ exprCstrSite' cstrSite -- Wrapping all construction sites in ExprNodes is okay, because we use anyNode as parser (in reparseNestedCstrSites)
        reparse :: forall n.
            (Node ~ NodeOf n, Data n, Decomposable n)
            => Parser n
            -> n
            -> (n, Set ParseError)
        reparse parser node
            = uncurry (reparseNestedCstrSites @Program reparse)
            . (\cstrSite ->
               either (\errors -> ((cstrSite, node), fromFoldable errors))
                      (\newNode -> ((cstrSite, newNode), mempty))
               $ runParser @Program parser cstrSite)
            $ decompose node

unsafePrettyProgram :: Program -> Doc PrettyAnnotation
unsafePrettyProgram (ProgramCstrSite _ contents)
    = prettyCstrSite undefined annPretty contents -- should be safe, because root construction site annotation is removed
unsafePrettyProgram Program{..}
    = annPretty expr <> nest 2 (line' <> annPretty whereClause)

instance Decomposable Program where
    conservativelyDecompose _ _ = Nothing
    traverseComponents mapChar mapNode program@Program{}
        = chainDisJoint program
        $ Disjoint (intersperseWhitespaceTraversers
                        mapChar
                        program
                        [ Traverser' #expr mapNode
                        , Traverser' (#whereClause % _Just) mapNode
                        ]
                    :> Traverser' (#meta % #trailingWhitespace)
                                  (unpacked % traversed %%~ mapChar))
    traverseComponents mapChar mapNode (ProgramCstrSite meta materials)
        = ProgramCstrSite meta <$> traverseComponents mapChar mapNode materials

instance DisplayProjection Program

instance Validity Program where
    validate
        = mconcat [ genericValidate
                  , validateInterstitialWhitespace validInterstitialWhitespace
                  , maybe valid hasNonEmptyInterstitialWhitespace
                    . guarded (is $ #whereClause % _Just)
                  , maybe valid
                          (validateInterstitialWhitespaceWith
                               (declare "is empty" . Text.null))
                    . guarded (is $ #whereClause % _Nothing)
                  ]

instance ValidInterstitialWhitespace Program where
    validInterstitialWhitespace = \case
        Program{..} -> if isJust whereClause then 1 else 0
        ProgramCstrSite{} -> 0

instance KnownNat s => GenValid (Sized s Program) where
    genValid = sized uniformValid
    shrinkValid Sized{..}
        = Sized size <$> shrinkValidStructurallyWithoutExtraFiltering unSized -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

enumerateValidProgram
    :: (Typeable f, Control.Sized.Sized f) => Int -> Shareable f Program
enumerateValidProgram size
    = aconcat
        [ Program <$> enumerateValidProgramMeta 0
          <*> accessValid
          <*> splurge (size `div` 3) (pure Nothing) -- appropriate cost is dependent on total size
        , Program .: setInterstitialWhitespace <$> accessValid
          <*> enumerateValidProgramMeta 0
          <*> accessValid
          <*> (Just <$> accessValid)
        , addMetaWith enumerateValidProgramMeta ProgramCstrSite
        ]
  where
    setInterstitialWhitespace
        :: NonEmpty Whitespace -> ProgramMeta -> ProgramMeta
    setInterstitialWhitespace interstitialWhitespace
        = #standardMeta % #interstitialWhitespace
        .~ [ toText . map unWhitespace $ toList interstitialWhitespace ]

instance KnownNat s => ValidEnumerable (Sized s Program) where
    enumerateValid
        = datatype
            [ Sized size <$> enumerateValidProgram (fromEnum $ natVal size) ]
      where
        size = Proxy :: Proxy s
