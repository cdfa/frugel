{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Node where

import Control.Limited
import Control.Monad.Writer         ( Writer )
import Control.Sized
import Control.ValidEnumerable
import Control.ValidEnumerable.Whitespace

import Data.Alphanumeric
import Data.Composition
import Data.Data
import Data.GenValidity
import Data.GenValidity.Sequence    ()
import Data.Has
import Data.Hidden
import Data.MultiSet                ( MultiSet )
import qualified Data.Text          as Text
import Data.Text.Optics
import Data.Validity.Sequence       ()

import Frugel
import Frugel.DisplayProjection     as DisplayProjection

import Numeric.Optics

import Optics.Extra

import Scout.Internal.Meta          ( ExprMeta(standardMeta) )
import qualified Scout.Internal.Meta
import Scout.Meta

import Test.QuickCheck.Gen          as Gen

type CstrSite = ACstrSite Node

data Node = ExprNode Expr | DeclNode Decl | WhereNode WhereClause
    deriving ( Eq, Ord, Show, Generic, Data )

newtype Identifier = Identifier (NonEmpty Alphanumeric)
    deriving ( Eq, Ord, Show, Generic, Data )

data Expr
    = Variable ExprMeta Identifier
    | Abstraction AbstractionMeta Identifier Expr
    | Application ExprMeta Expr Expr
    | Sum ExprMeta Expr Expr
    | ExprCstrSite ExprMeta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data )

data Decl
    = Decl { meta :: Meta, name :: Identifier, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

data WhereClause
    = WhereClause Meta (NonEmpty Decl) | WhereCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

data AbstractionMeta
    = AbstractionMeta { standardExprMeta :: ExprMeta
                      , reified :: Maybe (Hidden ReifiedFunction)
                      }
    deriving ( Eq, Ord, Show, Generic, Data, Has ExprMeta )

type instance NodeOf Node = Node

type instance NodeOf Identifier = Node

type instance NodeOf Expr = Node

type instance NodeOf Decl = Node

type instance NodeOf WhereClause = Node

type ReifiedFunction
    = ScopedEvaluation Expr -> (LimiterT ScopedEvaluation) Expr

-- For making explicit that something should not be given a environment, but gets it from it's scope
-- Use MultiSets until errors have locations (probably easiest to do with abstract syntax graph with error nodes)
type ScopedEvaluation = Writer (MultiSet EvaluationError)

data EvaluationError
    = TypeError TypeError
    | UnboundVariableError Identifier
    | ConflictingDefinitionsError Identifier
    | OutOfFuelError Expr
    deriving ( Eq, Show, Ord, Data )

data TypeError = TypeMismatchError ExpectedType Expr
    deriving ( Eq, Show, Ord, Data )

data ExpectedType = Function | Integer
    deriving ( Eq, Show, Ord, Data )

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makeFieldLabelsWith noPrefixFieldLabels ''AbstractionMeta

makePrisms ''Node

makePrisms ''Identifier

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

instance Has ExprMeta Expr where
    getter (Abstraction AbstractionMeta{..} _ _) = standardExprMeta
    getter (Variable meta _) = meta
    getter (Application meta _ _) = meta
    getter (Sum meta _ _) = meta
    getter (ExprCstrSite meta _) = meta
    modifier
        = over ((_Abstraction % _1 % #standardExprMeta)
                `adjoin` (_Variable % _1)
                `adjoin` (_Application % _1)
                `adjoin` (_Sum % _1)
                `adjoin` (_ExprCstrSite % _1))

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens

declMeta :: Lens' Decl Meta
declMeta = hasLens

whereClauseMeta :: Lens' WhereClause Meta
whereClauseMeta = hasLens

exprCstrSite' :: CstrSite -> Expr
exprCstrSite' = ExprCstrSite $ defaultExprMeta 0

declCstrSite' :: CstrSite -> Decl
declCstrSite' = DeclCstrSite $ defaultMeta 0

whereCstrSite' :: CstrSite -> WhereClause
whereCstrSite' = WhereCstrSite $ defaultMeta 0

instance IsNode Node

instance IsNode Expr

instance IsNode Decl

instance IsNode WhereClause

instance NodePrism Node where
    nodePrism = castOptic simple

instance NodePrism Expr where
    nodePrism = _ExprNode

instance NodePrism Decl where
    nodePrism = _DeclNode

instance NodePrism WhereClause where
    nodePrism = _WhereNode

instance CstrSiteNode Node where
    setCstrSite cstrSite = \case
        ExprNode expr -> ExprNode $ setCstrSite cstrSite expr
        DeclNode expr -> DeclNode $ setCstrSite cstrSite expr
        WhereNode expr -> WhereNode $ setCstrSite cstrSite expr
    _NodeCstrSite
        = singular
        $ (_ExprNode % _NodeCstrSite)
        `adjoin` (_DeclNode % _NodeCstrSite)
        `adjoin` (_WhereNode % _NodeCstrSite)

instance CstrSiteNode Expr where
    setCstrSite = const . exprCstrSite'
    _NodeCstrSite
        = _ExprCstrSite
        % unsafeFiltered
            ((== 0) . view (_1 % #parenthesisLevels)) -- safe, because value with predicate is disjoint from focus
        % _2

instance CstrSiteNode Decl where
    setCstrSite = const . declCstrSite'
    _NodeCstrSite = _DeclCstrSite % _2

instance CstrSiteNode WhereClause where
    setCstrSite = const . whereCstrSite'
    _NodeCstrSite = _WhereCstrSite % _2

instance ToString Identifier where
    toString (Identifier name) = map unAlphanumeric $ toList name

instance Pretty Identifier where
    pretty = pretty . toString

instance DisplayProjection Node where
    -- _NodeCstrSite of Node finds construction sites from the nodes and would skip any overridden renderDoc definitions, though there are none now
    renderDoc (ExprNode expr) = renderDoc expr
    renderDoc (DeclNode decl) = renderDoc decl
    renderDoc (WhereNode whereClause) = renderDoc whereClause

instance DisplayProjection Expr where
    renderDoc e
        = if e ^. exprMeta % #standardMeta % #elided
          then DisplayProjection.annotate Elided "<ExprNode>"
          else defaultRenderDoc e

instance DisplayProjection Decl where
    renderDoc decl
        = if decl ^. declMeta % #elided
          then DisplayProjection.annotate Elided "<DeclNode>"
          else defaultRenderDoc decl

instance DisplayProjection WhereClause where
    renderDoc whereClause
        = if whereClause ^. whereClauseMeta % #elided
          then DisplayProjection.annotate Elided "<WhereClauseNode>"
          else defaultRenderDoc whereClause

instance DisplayProjection EvaluationError where
    renderDoc = \case
        TypeError e -> "Type error:" <+> renderDoc e
        UnboundVariableError name -> pretty name <+> "was not defined"
        ConflictingDefinitionsError name ->
            pretty name <+> "was defined multiple times in a the same scope"
        OutOfFuelError expr -> "Ran out of fuel when calculating:"
            `nestingLine` annotateComplete (renderDoc expr)

instance DisplayProjection TypeError where
    renderDoc (TypeMismatchError expected expr)
        = "Expected type"
        <+> pretty expected <> line <> "does not match"
        <+> annotateComplete (renderDoc expr)

instance Pretty ExpectedType where
    pretty = viaShow

instance Decomposable Node where
    traverseComponents mapChar mapNode = \case
        ExprNode expr -> ExprNode <$> traverseComponents mapChar mapNode expr
        DeclNode decl -> DeclNode <$> traverseComponents mapChar mapNode decl
        WhereNode whereClause ->
            WhereNode <$> traverseComponents mapChar mapNode whereClause

instance Decomposable Identifier where
    conservativelyDecompose _ _ = Nothing
    traverseComponents mapChar _ identifier@(Identifier _)
        = traverseOf (_Identifier % traversed % #unAlphanumeric)
                     mapChar
                     identifier

instance Decomposable Expr where
    traverseComponents mapChar mapNode e
        | e ^. exprMeta % #parenthesisLevels > 0
            = chainDisJoint e
            $ Disjoint
                [ keyWordCharTraversal mapChar '('
                , whitespaceFragmentTraverser _head mapChar
                , Traverser'
                      (refracting (exprMeta % #parenthesisLevels)
                                  (subtracting 1)
                       % refracting
                           (exprMeta % #standardMeta % #interstitialWhitespace)
                           (_tail % _init))
                      mapNode
                , whitespaceFragmentTraverser _last mapChar
                , keyWordCharTraversal mapChar ')'
                ]
    traverseComponents mapChar mapNode e
        = chainDisJoint e
        . Disjoint
        . intersperseWhitespaceTraversers mapChar e
        $ case e of
            -- All these cases could be composed into 1, because the lenses don't overlap, but this is better for totality checking
            Variable{} -> [ Traverser' (_Variable % _2)
                            $ traverseComponents mapChar mapNode
                          ]
            Abstraction{} -> [ keyWordCharTraversal mapChar '\\'
                             , Traverser' (_Abstraction % _2)
                               $ traverseComponents mapChar mapNode
                             , keyWordCharTraversal mapChar '='
                             , Traverser' (_Abstraction % _3) mapNode
                             ]
            Application{} -> [ Traverser' (_Application % _2) mapNode
                             , Traverser' (_Application % _3) mapNode
                             ]
            Sum{} -> [ Traverser' (_Sum % _2) mapNode
                     , keyWordCharTraversal mapChar '+'
                     , Traverser' (_Sum % _3) mapNode
                     ]
            ExprCstrSite{} -> [ Traverser' (_ExprCstrSite % _2)
                                $ traverseComponents mapChar mapNode
                              ]

instance Decomposable Decl where
    traverseComponents mapChar mapNode decl@Decl{}
        = chainDisJoint decl . Disjoint
        $ intersperseWhitespaceTraversers
            mapChar
            decl
            [ Traverser' #name (traverseComponents mapChar mapNode)
            , keyWordCharTraversal mapChar '='
            , Traverser' #value mapNode
            ]
    traverseComponents mapChar mapNode (DeclCstrSite meta materials)
        = DeclCstrSite meta <$> traverseComponents mapChar mapNode materials

instance Decomposable WhereClause where
    traverseComponents mapChar mapNode whereClause@(WhereClause _ decls)
        = chainDisJoint whereClause . Disjoint
        $ intersperseWhitespaceTraversers
            mapChar
            whereClause
            (Traverser' (castOptic united) (<$ traverse_ @[] mapChar "where")
             : imap (\i _ -> Traverser' (_WhereClause % _2 % ix i) mapNode)
                    (toList decls))
    traverseComponents mapChar mapNode (WhereCstrSite meta materials)
        = WhereCstrSite meta <$> traverseComponents mapChar mapNode materials

keyWordCharTraversal
    :: (Is A_Lens k, Functor f) => (t -> f b) -> t -> Traverser' f k NoIx a
keyWordCharTraversal mapChar c = Traverser' (castOptic united) (<$ mapChar c)

intersperseWhitespaceTraversers :: (Applicative f, Has Meta n)
    => (Char -> f Char)
    -> n
    -> [Traverser' f An_AffineTraversal NoIx n]
    -> [Traverser' f An_AffineTraversal NoIx n]
intersperseWhitespaceTraversers mapChar n traversers
    = interleave [ traversers
                 , imap (\i _ -> whitespaceFragmentTraverser (ix i) mapChar)
                   $ view (hasLens @Meta % #interstitialWhitespace) n
                 ]

whitespaceFragmentTraverser
    :: (Has Meta s, Is l An_AffineTraversal, Applicative f)
    => Optic' l is [Text] Text
    -> (Char -> f Char)
    -> Traverser' f An_AffineTraversal is s
whitespaceFragmentTraverser selector mapChar
    = Traverser' (hasLens @Meta
                  % #interstitialWhitespace
                  % castOptic @An_AffineTraversal selector)
                 (unpacked % traversed %%~ mapChar)

parenthesizeExpr :: (a -> a) -> (Expr -> a) -> Expr -> a
parenthesizeExpr parenthesize prettyExpr x
    | x ^. exprMeta % #parenthesisLevels > 0
        = parenthesize
        $ parenthesizeExpr parenthesize
                           prettyExpr
                           (x & exprMeta % #parenthesisLevels -~ 1)
parenthesizeExpr _ prettyExpr x = prettyExpr x

class ValidInterstitialWhitespace a where
    validInterstitialWhitespace :: a -> Int

instance ValidInterstitialWhitespace Expr where
    validInterstitialWhitespace
        e = view (hasLens @ExprMeta % #parenthesisLevels) e * 2 + case e of
        Variable{} -> 0
        Abstraction{} -> 3
        Application{} -> 1
        Sum{} -> 2
        ExprCstrSite{} -> 0

instance ValidInterstitialWhitespace Decl where
    validInterstitialWhitespace = \case
        Decl{} -> 2
        DeclCstrSite{} -> 0

instance ValidInterstitialWhitespace WhereClause where
    validInterstitialWhitespace = \case
        WhereClause _ decls -> length decls
        WhereCstrSite{} -> 0

instance Validity Node

instance Validity Identifier

instance Validity Expr where
    validate
        = mconcat
            [ genericValidate
            , validateInterstitialWhitespace validInterstitialWhitespace
            , declare "has non-empty center whitespace fragment"
              . fromMaybe True
              . preview
                  (_Application
                   % _1
                   % #standardMeta
                   % #interstitialWhitespace
                   % to (maybe False (not . Text.null) . \whitespaceFragments ->
                         guard (odd $ length whitespaceFragments)
                         *> whitespaceFragments
                         !!? (length whitespaceFragments `div` 2)))
            ]

instance Validity Decl where
    validate
        = mconcat [ genericValidate
                  , validateInterstitialWhitespace validInterstitialWhitespace
                  ]

instance Validity WhereClause where
    validate
        = mconcat [ genericValidate
                  , validateInterstitialWhitespace validInterstitialWhitespace
                  , hasNonEmptyInterstitialWhitespace
                  ]

instance Validity AbstractionMeta

instance GenValid Node where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Identifier where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Expr where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance GenValid Decl where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance GenValid WhereClause where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance GenValid AbstractionMeta where
    genValid = sized . uniformWith $ enumerateValidAbstractionMeta 0
    shrinkValid absMeta@AbstractionMeta{..}
        = map (flip (set #standardExprMeta) absMeta)
        $ shrinkValidStructurallyWithoutExtraFiltering standardExprMeta -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance ValidEnumerable Node where
    enumerateValid = datatype [ c1 ExprNode, c1 DeclNode, c1 WhereNode ]

instance ValidEnumerable Identifier where
    enumerateValid
        = datatype [ Identifier .: (:|) <$> accessValid
                     <*> inflation ((2 ^) . (`div` 5)) [] ((:) <$> accessValid)
                   ]

instance ValidEnumerable Expr where
    enumerateValid
        = datatype
            [ Variable <$> enumerateValidExprMeta 0 <*> accessValid
            , splurge 2
              $ Abstraction <$> enumerateValidAbstractionMeta 3
              <*> accessValid
              <*> accessValid
            , Application .: setCenterWhitespace <$> accessValid
              <*> enumerateValidExprMeta 0
              <*> accessValid
              <*> accessValid
            , Sum <$> enumerateValidExprMeta 2 <*> accessValid <*> accessValid
            , ExprCstrSite <$> enumerateValidExprMeta 0 <*> accessValid
            ]
      where
        setCenterWhitespace nonEmptyWhitespace
            = #standardMeta % #interstitialWhitespace %~ \whitespaceFragments ->
            insertAt (length whitespaceFragments `div` 2)
                     (toText . map unWhitespace
                      $ toList @(NonEmpty _) nonEmptyWhitespace)
                     whitespaceFragments

instance ValidEnumerable Decl where
    enumerateValid
        = datatype [ addMeta (uncurry . Decl), addMeta DeclCstrSite ]

instance ValidEnumerable WhereClause where
    enumerateValid
        = datatype
            [ (\decls -> WhereClause
                   Meta { interstitialWhitespace = map (toText
                                                        . map unWhitespace
                                                        . toList @(NonEmpty _)
                                                        . fst)
                              $ toList decls
                        , elided = False
                        }
               $ fmap snd decls) <$> accessValid, addMeta WhereCstrSite ]

enumerateValidAbstractionMeta
    :: (Typeable f, Sized f) => Int -> Shareable f AbstractionMeta
enumerateValidAbstractionMeta n
    = pay $ AbstractionMeta <$> enumerateValidExprMeta n ?? Nothing

-- Not generally safe, see note `addMetaWith`
addMeta
    :: (ValidInterstitialWhitespace n, Sized f, Typeable f, ValidEnumerable a)
    => (Meta -> a -> n)
    -> Shareable f n
addMeta = addMetaWith enumerateValidMeta

-- Only safe when `validInterstitialWhitespace` does not evaluate anything else but the constructor
addMetaWith
    :: (ValidInterstitialWhitespace n, Sized f, Typeable f, ValidEnumerable a)
    => (Int -> Shareable f m)
    -> (m -> a -> n)
    -> Shareable f n
addMetaWith enumerateValidNodeMeta c
    = flip c <$> accessValid
    <*> (enumerateValidNodeMeta
         . validInterstitialWhitespace
         . c (error "Default meta was evaluated during enumeration")
         $ error "Dummy node children evaluated during enumeration")
