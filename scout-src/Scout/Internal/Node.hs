{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Node where

import qualified Control.Lens    as Lens
import Control.Limited
import Control.Monad.Writer      ( WriterT )
import Control.Sized
import Control.ValidEnumerable

import Data.Alphanumeric
import Data.Char
import Data.Composition
import Data.Data                 ( Data )
import Data.Data.Lens
import Data.GenValidity
import Data.GenValidity.Sequence ()
import Data.GenValidity.Text     ()
import Data.Has
import Data.Hidden
import Data.List                 ( findIndex )
import Data.MultiSet             ( MultiSet )
import qualified Data.Text       as Text
import Data.Text.Optics
import Data.Validity.Extra
import Data.Validity.Sequence    ()
import Data.Whitespace

import Frugel                    hiding ( Elided )
import Frugel.CstrSite.ValidEnumerable ()

import Generic.Data

import Numeric.Optics

import Optics.Extra.Scout

import PrettyPrinting.Expr

import qualified Relude.Unsafe   as Unsafe

import Scout.Operators           as Operators
import Scout.Orphans.Stream      ()

import Test.QuickCheck.Gen       as QuickCheck hiding ( vectorOf )
import Test.QuickCheck.Modifiers

type CstrSite = ACstrSite Node

data Node = ExprNode Expr | DeclNode Decl | WhereNode WhereClause
    deriving ( Eq, Ord, Show, Generic, Data )

newtype Identifier = Identifier (NonEmpty Alphanumeric)
    deriving ( Eq, Ord, Show, Generic, Data )
    deriving newtype ( Semigroup )

data Expr
    = Variable ExprMeta Identifier
    | Abstraction AbstractionMeta Identifier Expr
    | Application ExprMeta Expr Expr
    | BinaryOperation ExprMeta Expr BinaryOperator Expr
    | UnaryOperation ExprMeta UnaryOperator Expr
    | Literal ExprMeta Literal
    | ExprCstrSite ExprMeta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data )

data Literal = Boolean Bool | Integer Integer
    deriving ( Eq, Ord, Show, Generic, Data )

data Decl
    = Decl { meta :: Meta, name :: Identifier, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

data WhereClause
    = WhereClause Meta (NonEmpty Decl) | WhereCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

type instance NodeOf Node = Node

type instance NodeOf Identifier = Node

type instance NodeOf Expr = Node

type instance NodeOf Literal = Node

type instance NodeOf Decl = Node

type instance NodeOf WhereClause = Node

-- This has to live her instead of in Meta because ReifiedFunction depends on Expr
-- Could be removed by using data-diverse for Meta
data AbstractionMeta
    = AbstractionMeta { standardExprMeta :: ExprMeta
                      , reified :: Maybe (Hidden ReifiedFunction)
                      }
    deriving ( Eq, Ord, Show, Generic, Data, Has ExprMeta )

data ExprMeta
    = ExprMeta { standardMeta :: Meta
               , parenthesisLevels :: Int
               , evaluationStatus :: EvaluationStatus
               }
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

-- It would be nicer if GADTs would be used instead and the AST would be parametric in it's recursion (a la hypertypes), especially since elided and focused are not used by core Frugel
-- This would prevent derivation of Generic though
-- Meta had it's own modules someday, but the introduction of focusedNodeEvaluations requires it's in here. Until there is time to pull in data-diverse
data Meta
    = Meta { interstitialWhitespace :: [Text] -- Invariant: the number of whitespace fragments should be equal to the number of places in a node where whitespace can exist
             -- ATM this is only set to true by evaluation and obeyed by pretty printing (not standard rendering)
           , elided :: Bool
             -- This is not the source of truth for the cursor location (that's in Model). This is used in evaluation to check if the node is focused
           , focused :: Bool
           }
    deriving ( Eq, Ord, Show, Generic, Data )

data EvaluationStatus
    = Evaluated
    | EvaluationDeferred (Hidden ( EvaluationRef Expr
                                 , ScopedEvaluation Expr
                                   -> ScopedEvaluation Expr
                                 ))
    | Elided (Hidden Expr)
    | OutOfFuel
    deriving ( Eq, Ord, Show, Generic, Data )

type ReifiedFunction
    = EvaluationRef Expr
    -> LimiterT (ReaderT ShadowingEnv ScopedEvaluation) Expr

type EvaluationRef a = IORef (Either (ScopedEvaluation a) a)

-- For making explicit that something should not be given a environment, but gets it from it's scope
-- Use MultiSets until errors have locations (probably easiest to do with abstract syntax graph with error nodes)
type ScopedEvaluation = WriterT EvaluationOutput IO

type ShadowingEnv = Map Identifier Int

-- This lives here because of EvaluationError
-- Note that the output generated by evaluation of an expression to a function value is discarded when that function is applied to an argument
data EvaluationOutput
    = EvaluationOutput { errors :: MultiSet EvaluationError
                         -- Be careful with evaluating the Nodes completely because they may be infinitely large. Use capTree to cap them to a certain depth
                       , focusedNodeEvaluations :: Seq FocusedNodeEvaluation
                       }
    deriving ( Eq, Ord, Show, Generic, Data )
    deriving ( Semigroup, Monoid ) via (Generically EvaluationOutput)

data FocusedNodeEvaluation
    = FocusedNodeEvaluation { definitions :: Map Identifier Expr
                            , variables :: Map Identifier Expr
                            , value :: Node
                            }
    deriving ( Eq, Ord, Show, Generic, Data )

-- This lives here because of dependency on Expr
data EvaluationError
    = TypeError TypeError
    | UnboundVariableError Identifier
    | ConflictingDefinitionsError Identifier
    | OutOfFuelError Expr
    | DivideByZeroError
    deriving ( Eq, Show, Ord, Data )

data TypeError
    = TypeValueMismatch ExpectedType Expr
    | LiteralTypesMismatch Literal Literal
    deriving ( Eq, Show, Ord, Data )

data ExpectedType = FunctionType | IntegerType | BoolType | AnyType
    deriving ( Eq, Show, Ord, Data )

makeFieldLabelsNoPrefix ''Decl

makeFieldLabelsNoPrefix ''AbstractionMeta

makePrisms ''Node

makePrisms ''Identifier

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

makeFieldLabelsNoPrefix ''ExprMeta

makeFieldLabelsNoPrefix ''Meta

makePrisms ''EvaluationStatus

makeFieldLabelsNoPrefix ''EvaluationOutput

makeFieldLabelsNoPrefix ''FocusedNodeEvaluation

makePrisms ''FocusedNodeEvaluation

instance LabelOptic "exprMeta" An_AffineFold Node Node ExprMeta ExprMeta where
    labelOptic = castOptic $ noIx ignored

instance LabelOptic "exprMeta" An_AffineFold Expr Expr ExprMeta ExprMeta where
    labelOptic = castOptic hasLens

instance LabelOptic "exprMeta" An_AffineFold Decl Decl ExprMeta ExprMeta where
    labelOptic = castOptic $ noIx ignored

instance LabelOptic "exprMeta" An_AffineFold WhereClause WhereClause ExprMeta ExprMeta where
    labelOptic = castOptic $ noIx ignored

instance LabelOptic "context" A_Traversal FocusedNodeEvaluation FocusedNodeEvaluation Expr Expr where
    labelOptic = #definitions % traversed `adjoin` #variables % traversed

instance Has Meta Node where
    getter (ExprNode n) = getter n
    getter (DeclNode n) = getter n
    getter (WhereNode n) = getter n
    modifier f n@(ExprNode _) = n & _ExprNode %~ modifier f
    modifier f n@(DeclNode _) = n & _DeclNode %~ modifier f
    modifier f n@(WhereNode _) = n & _WhereNode %~ modifier f

instance Has ExprMeta Expr where
    getter (Abstraction AbstractionMeta{..} _ _) = standardExprMeta
    getter (Variable meta _) = meta
    getter (Application meta _ _) = meta
    getter (BinaryOperation meta _ _ _) = meta
    getter (UnaryOperation meta _ _) = meta
    getter (Literal meta _) = meta
    getter (ExprCstrSite meta _) = meta
    modifier = over . singular $ traversalVL $ template @_ @ExprMeta

instance Has Meta Expr where
    getter = standardMeta . getter
    modifier = over (exprMeta % #standardMeta)

instance Has Meta AbstractionMeta where
    getter = getter . standardExprMeta
    modifier = over (#standardExprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens

-- declMeta :: Lens' Decl Meta
-- declMeta = hasLens
-- whereClauseMeta :: Lens' WhereClause Meta
-- whereClauseMeta = hasLens
exprCstrSite' :: CstrSite -> Expr
exprCstrSite' = ExprCstrSite $ defaultExprMeta 0

declCstrSite' :: CstrSite -> Decl
declCstrSite' = DeclCstrSite $ defaultMeta 0

whereCstrSite' :: CstrSite -> WhereClause
whereCstrSite' = WhereCstrSite $ defaultMeta 0

defaultExprMeta :: Int -> ExprMeta
defaultExprMeta n
    = ExprMeta { parenthesisLevels = 0
               , evaluationStatus = Evaluated
               , standardMeta = defaultMeta n
               }

defaultMeta :: Int -> Meta
defaultMeta n
    = Meta { interstitialWhitespace = replicate n ""
           , elided = False
           , focused = False
           }

-- Note that expressions may have the precedence of literals when parenthesized
instance Expression Expr where
    precedence e = case e of
        (BinaryOperation _ _ binOp _) -> precedence' e
            + fromMaybe missingOperatorError
                        (findIndex (elem binOp) binaryOperatorPrecedence)
          where
            missingOperatorError
                = error
                $ show binOp <> " was not in the operator precedence list"
        _ -> if precedence' e > 2
            then precedence' e + length binaryOperatorPrecedence - 1
            else precedence' e
      where
        precedence' :: Expr -> Int
        precedence' Abstraction{} = 4
        precedence' UnaryOperation{} = 3
        precedence' BinaryOperation{} = 2
        precedence' Application{} = 1
        precedence' Variable{} = 0
        precedence' Literal{} = 0
        precedence' ExprCstrSite{} = 0
    fixity Abstraction{} = Just Prefix
    fixity (UnaryOperation _ unOp _) = Just $ Operators.fixity unOp
    fixity Application{} = Just Infix
    fixity BinaryOperation{} = Just Infix
    fixity Variable{} = Nothing
    fixity Literal{} = Nothing
    fixity ExprCstrSite{} = Nothing
    associativity Application{} = Just LeftAssociative
    associativity (BinaryOperation _ _ binOp _)
        = Just $ Operators.associativity binOp
    associativity UnaryOperation{} = Nothing
    associativity Abstraction{} = Nothing
    associativity Variable{} = Nothing
    associativity Literal{} = Nothing
    associativity ExprCstrSite{} = Nothing

parenthesizeExprFromMeta :: (a -> a) -> (Expr -> a) -> Expr -> a
parenthesizeExprFromMeta parenthesize prettyExpr x
    = nTimes (x ^. exprMeta % #parenthesisLevels) parenthesize $ prettyExpr x

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

instance Pretty Literal where
    pretty (Boolean b) = viaShow b
    pretty (Integer i) = viaShow i

instance Pretty EvaluationStatus where
    pretty EvaluationDeferred{} = angles "EvaluationDeferred"
    pretty Elided{} = "..."
    pretty status = angles $ viaShow status

instance DisplayProjection Node where
    -- _NodeCstrSite of Node finds construction sites from the nodes and would skip any overridden renderDoc definitions, though there are none now
    renderDoc (ExprNode expr) = renderDoc expr
    renderDoc (DeclNode decl) = renderDoc decl
    renderDoc (WhereNode whereClause) = renderDoc whereClause

-- At the moment, `renderDoc e` will not place additional parentheses to ensure the rendered program reflects the AST according to the grammar and associativity/fixity of the operators
-- This functionality could be copied from the pretty printing definition if needed
instance DisplayProjection Expr

instance DisplayProjection Decl

instance DisplayProjection WhereClause

instance DisplayProjection Literal where
    renderDoc = \case
        Boolean b -> viaShow b
        Integer i -> viaShow i

instance Decomposable Node where
    traverseComponents traverseChar traverseNode = \case
        ExprNode expr ->
            ExprNode <$> traverseComponents traverseChar traverseNode expr
        DeclNode decl ->
            DeclNode <$> traverseComponents traverseChar traverseNode decl
        WhereNode whereClause -> WhereNode
            <$> traverseComponents traverseChar traverseNode whereClause

instance Decomposable Identifier where
    conservativelyDecompose _ _ = Nothing
    traverseComponents traverseChar _ identifier@(Identifier _)
        = traverseOf (_Identifier % traversed % #unAlphanumeric)
                     traverseChar
                     identifier

instance Decomposable Expr where
    traverseComponents traverseChar traverseNode e
        | e ^. exprMeta % #parenthesisLevels > 0
            = chainDisJoint e
            $ Disjoint
                [ keywordCharTraversal traverseChar '('
                , whitespaceFragmentTraverser _head traverseChar
                , Traverser'
                      (refracting (exprMeta % #parenthesisLevels)
                                  (subtracting 1)
                       % refracting
                           (exprMeta % #standardMeta % #interstitialWhitespace)
                           (_tail % _init))
                      traverseNode
                , whitespaceFragmentTraverser _last traverseChar
                , keywordCharTraversal traverseChar ')'
                ]
    traverseComponents traverseChar traverseNode e
        = chainDisJoint e
        . Disjoint
        . intersperseWhitespaceTraversers traverseChar e
        $ case e of
            -- All these cases could be composed into 1, because the lenses don't overlap, but this is better for totality checking
            Variable{} -> [ Traverser' (_Variable % _2)
                            $ traverseComponents traverseChar traverseNode
                          ]
            Abstraction{} -> [ keywordCharTraversal traverseChar '\\'
                             , Traverser' (_Abstraction % _2)
                               $ traverseComponents traverseChar traverseNode
                             , keywordCharTraversal traverseChar '='
                             , Traverser' (_Abstraction % _3) traverseNode
                             ]
            Application{} -> [ Traverser' (_Application % _2) traverseNode
                             , Traverser' (_Application % _3) traverseNode
                             ]
            (UnaryOperation _ unOp _) ->
                [ keywordStringTraversal traverseChar
                                         (unaryOperatorSymbol unOp)
                , Traverser' (_UnaryOperation % _3) traverseNode
                ]
            (BinaryOperation _ _ binOp _) ->
                [ Traverser' (_BinaryOperation % _2) traverseNode
                , keywordStringTraversal traverseChar
                                         (binaryOperatorSymbol binOp)
                , Traverser' (_BinaryOperation % _4) traverseNode
                ]
            Literal{} -> [ Traverser' (_Literal % _2) $ \case
                Boolean b -> Boolean b
                    <$ traverse traverseChar (show @String b)
                Integer i -> Integer i
                    <$ traverse traverseChar (show @String i) ]
            ExprCstrSite{} -> [ Traverser' (_ExprCstrSite % _2)
                                $ traverseComponents traverseChar traverseNode
                              ]

instance Decomposable Decl where
    traverseComponents traverseChar traverseNode decl@Decl{}
        = chainDisJoint decl . Disjoint
        $ intersperseWhitespaceTraversers
            traverseChar
            decl
            [ Traverser' #name (traverseComponents traverseChar traverseNode)
            , keywordCharTraversal traverseChar '='
            , Traverser' #value traverseNode
            ]
    traverseComponents traverseChar traverseNode (DeclCstrSite meta materials)
        = DeclCstrSite meta
        <$> traverseComponents traverseChar traverseNode materials

instance Decomposable WhereClause where
    traverseComponents traverseChar
                       traverseNode
                       whereClause@(WhereClause _ decls)
        = chainDisJoint whereClause . Disjoint
        $ intersperseWhitespaceTraversers
            traverseChar
            whereClause
            (Traverser' (castOptic united)
                        (<$ traverse_ @[] traverseChar "where")
             : imap (\i _ -> Traverser' (_WhereClause % _2 % ix i) traverseNode)
                    (toList decls))
    traverseComponents traverseChar traverseNode (WhereCstrSite meta materials)
        = WhereCstrSite meta
        <$> traverseComponents traverseChar traverseNode materials

keywordCharTraversal
    :: (Is A_Lens k, Functor f) => (t -> f b) -> t -> Traverser' f k NoIx a
keywordCharTraversal traverseChar c
    = Traverser' (castOptic united) (<$ traverseChar c)

keywordStringTraversal :: (Is A_Lens k, Applicative f)
    => (a -> f b)
    -> [a]
    -> Traverser' f k NoIx s
keywordStringTraversal traverseChar s
    = Traverser' (castOptic united) (<$ traverse_ @[] traverseChar s)

intersperseWhitespaceTraversers :: (Applicative f, Has Meta n)
    => (Char -> f Char)
    -> n
    -> [Traverser' f An_AffineTraversal NoIx n]
    -> [Traverser' f An_AffineTraversal NoIx n]
intersperseWhitespaceTraversers traverseChar n traversers
    = interleave
        [ traversers
        , imap (\i _ -> whitespaceFragmentTraverser (ix i) traverseChar)
          $ view (hasLens @Meta % #interstitialWhitespace) n
        ]

whitespaceFragmentTraverser
    :: (Has Meta s, Is l An_AffineTraversal, Applicative f)
    => Optic' l is [Text] Text
    -> (Char -> f Char)
    -> Traverser' f An_AffineTraversal is s
whitespaceFragmentTraverser selector traverseChar
    = Traverser' (hasLens @Meta
                  % #interstitialWhitespace
                  % castOptic @An_AffineTraversal selector)
                 (unpacked % traversed %%~ traverseChar)

class ValidInterstitialWhitespace a where
    validInterstitialWhitespace :: a -> Int

instance ValidInterstitialWhitespace Expr where
    validInterstitialWhitespace
        e = view (hasLens @ExprMeta % #parenthesisLevels) e * 2 + case e of
        Variable{} -> 0
        Abstraction{} -> 3
        Application{} -> 1
        UnaryOperation{} -> 0
        BinaryOperation{} -> 2
        Literal{} -> 0
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

instance Validity Identifier where
    validate
        = mconcat [ genericValidate
                  , declare "has alphabetic first character"
                    . isLetter
                    . unAlphanumeric
                    . head
                    . view _Identifier
                  ]

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
            , declare "does not have colliding non-associative operators"
              . Lens.nullOf
                  (parentheSizedChildren
                   . Lens.cosmosOf parentheSizedChildren
                   . Lens.filtered
                       (any ((== NotAssociative) . Operators.associativity)
                        . preview (_BinaryOperation % _3)))
            ]
      where
        parentheSizedChildren
            = uniplate
            . Lens.filtered
                ((== 0) . view (hasLens @ExprMeta % #parenthesisLevels))

instance Validity Literal

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

instance Validity ExprMeta where
    validate
        = mconcat
            [ genericValidate
            , decorate
                  "The number of surrounding parentheses (parenthesisLevels)"
              . declare "is greater than or equal to 0"
              . (>= 0)
              . parenthesisLevels
            , decorate "The standardMeta"
              . declare "The number of whitespace fragments is greater or equal then twice the number of surrounding parentheses (parenthesisLevels * 2)"
              . \ExprMeta{..} -> length (interstitialWhitespace standardMeta)
              >= parenthesisLevels
            ]

instance Validity Meta where
    validate
        = mconcat [ genericValidate
                  , decorate "The interstitial whitespace"
                    . flip decorateList validateWhitespace
                    . interstitialWhitespace
                  ]

instance Validity EvaluationStatus

instance Validity EvaluationOutput where
    validate _ = valid

validateInterstitialWhitespace :: Has Meta a => (a -> Int) -> a -> Validation
validateInterstitialWhitespace expectedWhitespaceFragmentCount n
    = mconcat [ genericValidate
              , declare "has the correct number of whitespace fragments"
                . (== expectedWhitespaceFragmentCount n)
                . length
                . interstitialWhitespace
              ]
    $ getter n

hasNonEmptyInterstitialWhitespace :: Has Meta a => a -> Validation
hasNonEmptyInterstitialWhitespace
    = validateInterstitialWhitespaceWith
        (declare "is not empty" . not . Text.null)

validateInterstitialWhitespaceWith
    :: Has Meta b => (Text -> Validation) -> b -> Validation
validateInterstitialWhitespaceWith extraValidation
    = decorate "Meta"
    . decorate "The interstitial whitespace"
    . flip decorateList (validateWhitespace <> extraValidation)
    . interstitialWhitespace
    . getter

instance GenValid Node where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Identifier where
    genValid = sized uniformValid
    shrinkValid = shrinkValid

instance GenValid Expr where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking Meta maintains the number of interstitial whitespace fragments

instance GenValid Literal where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No validity requirements

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

instance GenValid ExprMeta where
    genValid = QuickCheck.sized . uniformWith $ enumerateValidExprMeta 0
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required since shrinking Ints does not shrink to negative numbers

instance GenValid Meta where
    genValid = QuickCheck.sized . uniformWith $ enumerateValidMeta 0
    shrinkValid meta@Meta{interstitialWhitespace}
        = shrinkValidStructurallyWithoutExtraFiltering interstitialWhitespace
        -- ensure number of whitespace fragments is preserved
        & filter ((length interstitialWhitespace ==) . length)
        -- ensure only characters from previous whitespace fragments are used
        & mapped % imapped
        %@~ (\i whitespaceFragment -> Text.take (Text.length whitespaceFragment)
             $ interstitialWhitespace Unsafe.!! i)
        & fmap (flip (set #interstitialWhitespace) meta)

-- Not a proper GenValid instance, but generated nodes should not have random evaluation output
-- Instance is primarily provided to be able to use shrinkValidStructurallyWithoutExtraFiltering
instance GenValid EvaluationStatus where
    genValid = pure Evaluated
    shrinkValid = const []

instance ValidEnumerable Node where
    enumerateValid = datatype [ c1 ExprNode, c1 DeclNode, c1 WhereNode ]

instance ValidEnumerable Identifier where
    enumerateValid
        = datatype [ Identifier .: (:|) <$> accessLetter
                     <*> inflation ((2 ^) . (`div` 5)) [] ((:) <$> accessValid)
                   ]

instance ValidEnumerable Expr where
    enumerateValid
        = datatype
            [ Variable <$> enumerateValidExprMeta 0 <*> accessValid
            , Abstraction <$> enumerateValidAbstractionMeta 3
              <*> accessValid
              <*> accessValid
            , Application .: setCenterWhitespace <$> accessValid
              <*> enumerateValidExprMeta 0
              <*> accessValid
              <*> accessValid
            , UnaryOperation <$> enumerateValidExprMeta 0
              <*> accessValid
              <*> accessValid
            , binaryOperation' <$> enumerateValidExprMeta 2
              <*> accessValid
              <*> accessValid
              <*> accessValid
            , Literal <$> enumerateValidExprMeta 0 <*> accessValid
            , ExprCstrSite <$> enumerateValidExprMeta 0 <*> accessValid
            ]
      where
        setCenterWhitespace nonEmptyWhitespace
            = #standardMeta % #interstitialWhitespace %~ \whitespaceFragments ->
            insertAt (length whitespaceFragments `div` 2)
                     (toText . map unWhitespace
                      $ toList @(NonEmpty _) nonEmptyWhitespace)
                     whitespaceFragments
        -- Make all non-associative operations parenthesized, to prevent collisions
        binaryOperation' meta left binOp right
            | Operators.associativity binOp == NotAssociative
                = BinaryOperation (addParentheses meta) left binOp right
        binaryOperation' meta left binOp right
            = BinaryOperation meta left binOp right
        addParentheses meta@ExprMeta{..}
            = if parenthesisLevels == 0
              then meta
                  & #parenthesisLevels .~ 1
                  & #standardMeta % #interstitialWhitespace
                  %~ (("" :) . (++ [ "" ]))
              else meta

instance ValidEnumerable Literal where
    enumerateValid
        = datatype [ Boolean <$> accessValid
                   , Integer . getNonNegative <$> accessValid
                   ]

instance ValidEnumerable Decl where
    enumerateValid
        = datatype [ addMeta (uncurry . Decl), addMeta DeclCstrSite ]

instance ValidEnumerable WhereClause where
    enumerateValid
        = datatype
            [ (\decls -> WhereClause
                   (defaultMeta 0) { interstitialWhitespace = map
                                         (toText
                                          . map unWhitespace
                                          . toList @(NonEmpty _)
                                          . fst)
                                         $ toList decls
                                   }
               $ fmap snd decls) <$> accessValid, addMeta WhereCstrSite ]

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

enumerateValidAbstractionMeta
    :: (Typeable f, Sized f) => Int -> Shareable f AbstractionMeta
enumerateValidAbstractionMeta n
    = pay $ AbstractionMeta <$> enumerateValidExprMeta n ?? Nothing

enumerateValidExprMeta :: (Typeable f, Sized f) => Int -> Shareable f ExprMeta
enumerateValidExprMeta minimumWhitespaceFragments
    = pay
    $ (\meta' parenthesisWhitespace ->
       ExprMeta { parenthesisLevels = length parenthesisWhitespace
                , evaluationStatus = Evaluated
                , standardMeta = meta'
                      & #interstitialWhitespace
                      %~ (\whitespaceFragments -> map fst parenthesisWhitespace
                          ++ whitespaceFragments
                          ++ map snd parenthesisWhitespace)
                }) <$> enumerateValidMeta minimumWhitespaceFragments
    <*> inflation (2 ^)
                  []
                  ((:) .: (,) <$> enumerateWhitespace <*> enumerateWhitespace)

enumerateValidMeta :: (Typeable f, Sized f) => Int -> Shareable f Meta
enumerateValidMeta n
    = pay
    $ Meta <$> vectorOf n enumerateWhitespace <*> pure False <*> pure False
