{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

import Control.Enumerable.Combinators ()
import Control.ValidEnumerable
import Control.ValidEnumerable.Whitespace

import Data.Alphanumeric
import Data.Data
import Data.GenValidity
import Data.GenValidity.Sequence    ()
import Data.Has
import qualified Data.Text          as Text
import Data.Text.Optics
import Data.Validity.Sequence       ()

import Frugel.CstrSite
import Frugel.Decomposition
import Frugel.DisplayProjection
import Frugel.PrettyPrinting

import Numeric.Optics

import Optics.Extra

import Relude.Unsafe                ( (!!) )

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
    | Abstraction ExprMeta Identifier Expr
    | Application ExprMeta Expr Expr
    | Sum ExprMeta Expr Expr
    | ExprCstrSite ExprMeta CstrSite
    deriving ( Eq, Ord, Show, Generic, Data, Has ExprMeta )

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

type instance NodeOf Decl = Node

type instance NodeOf WhereClause = Node

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makePrisms ''Node

makePrisms ''Identifier

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

fromString :: String -> Maybe Identifier
fromString = Identifier <.> (nonEmpty <=< traverse fromChar)

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
    _NodeCstrSite = _ExprCstrSite % _2

instance CstrSiteNode Decl where
    setCstrSite = const . declCstrSite'
    _NodeCstrSite = _DeclCstrSite % _2

instance CstrSiteNode WhereClause where
    setCstrSite = const . whereCstrSite'
    _NodeCstrSite = _WhereCstrSite % _2

instance DisplayProjection Node where
    -- _NodeCstrSite of Node finds construction sites from the nodes and would skip any overridden renderDoc definitions
    renderDoc (ExprNode expr) = renderDoc expr
    renderDoc (DeclNode decl) = renderDoc decl
    renderDoc (WhereNode w) = renderDoc w

instance DisplayProjection Expr where
    -- Parentheses are handled here (instead of relying on decompose), because the information would be removed when the expression is a construction site
    renderDoc = either defaultRenderDoc parenthesize . unwrapParentheses
      where
        parenthesize (leadingFragment, e, trailingFragment)
            = parens (pretty leadingFragment
                      <> renderDoc e
                      <> pretty trailingFragment)

instance DisplayProjection Decl

instance DisplayProjection WhereClause

instance Decomposable Node where
    conservativelyDecompose cstrSiteOffset = \case
        ExprNode expr -> conservativelyDecompose cstrSiteOffset expr
        DeclNode decl -> conservativelyDecompose cstrSiteOffset decl
        WhereNode whereClause ->
            conservativelyDecompose cstrSiteOffset whereClause
    traverseComponents mapChar mapNode = \case
        ExprNode expr -> ExprNode <$> traverseComponents mapChar mapNode expr
        DeclNode decl -> DeclNode <$> traverseComponents mapChar mapNode decl
        WhereNode whereClause ->
            WhereNode <$> traverseComponents mapChar mapNode whereClause

instance Decomposable Identifier where
    traverseComponents mapChar _ identifier@(Identifier _)
        = traverseOf (_Identifier % traversed % #unAlphanumeric)
                     mapChar
                     identifier

instance Decomposable Expr where
    conservativelyDecompose
        = conservativelyDecomposeNode _ExprNode (_ExprCstrSite % _2)
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
    conservativelyDecompose
        = conservativelyDecomposeNode _DeclNode (_DeclCstrSite % _2)
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
    conservativelyDecompose
        = conservativelyDecomposeNode _WhereNode (_WhereCstrSite % _2)
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

instance AnnotatedPretty Node where
    annPretty (ExprNode expr) = annPretty expr
    annPretty (DeclNode decl) = annPretty decl
    annPretty (WhereNode w) = annPretty w

instance AnnotatedPretty Identifier where
    annPretty (Identifier name) = pretty name

instance AnnotatedPretty Expr where
    annPretty = parenthesizeExpr parens annPretty'
      where
        annPretty' (Variable _ n) = annPretty n

        annPretty' (Abstraction _ arg expr)
            = (backslash <> annPretty arg) `nestingLine` equals
            <+> annPretty expr
        annPretty' (Application _ function arg)
            = annPretty function `nestingLine` annPretty arg
        annPretty' (Sum _ left right)
            = annPretty left `nestingLine` "+" <+> annPretty right
        annPretty' (ExprCstrSite _ contents) = annPretty contents

instance AnnotatedPretty Decl where
    annPretty Decl{..}
        = annPretty name `nestingLine` equals <+> annPretty value
    annPretty (DeclCstrSite _ contents) = annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty (WhereCstrSite _ contents) = annPretty contents
    annPretty (WhereClause _ decls)
        = nest 2
               (line'
                <> "where"
                <> nest 2 (line <> vsep (map annPretty $ toList decls)))

parenthesizeExpr :: (a -> a) -> (Expr -> a) -> Expr -> a
parenthesizeExpr parenthesize prettyExpr x
    | x ^. exprMeta % #parenthesisLevels > 0
        = parenthesize
        $ parenthesizeExpr parenthesize
                           prettyExpr
                           (x & exprMeta % #parenthesisLevels -~ 1)
parenthesizeExpr _ prettyExpr x = prettyExpr x

unwrapParentheses :: Expr -> Either Expr (Text, Expr, Text)
unwrapParentheses e
    | e ^. exprMeta % #parenthesisLevels > 0
        = Right ( leadingFragment
                , e
                  & exprMeta % #parenthesisLevels -~ 1
                  & exprMeta % #standardMeta % #interstitialWhitespace
                  .~ middleWhitespaceFragments
                , trailingFragment
                )
  where
    (leadingFragment, (middleWhitespaceFragments, trailingFragment))
        = fromMaybe
            (error ("Encountered incorrect number of whitespace fragments in "
                    <> show e))
        $ preview (exprMeta
                   % #standardMeta
                   % #interstitialWhitespace
                   % _Cons
                   % ((,) <$^> _1 <*^> _2 % _Snoc))
                  e
unwrapParentheses e = Left e

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
            , declare "has non-empty middle whitespace"
              . fromMaybe True
              . preview (_Application
                         % _1
                         % #standardMeta
                         % #interstitialWhitespace
                         % to (not
                               . Text.null
                               . (\whitespaceFragments -> whitespaceFragments
                                  !! (length whitespaceFragments `div` 2 + 1))))
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
                  ]

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

instance ValidEnumerable Node where
    enumerateValid = datatype [ c1 ExprNode, c1 DeclNode, c1 WhereNode ]

instance ValidEnumerable Identifier where
    enumerateValid = datatype [ c1 Identifier ]

instance ValidEnumerable Expr where
    enumerateValid
        = datatype
            [ addExprMeta 0 Variable
            , addExprMeta 3 (uncurry . Abstraction)
            , addExprMeta
                  1
                  (uncurry
                   . Application
                   -- Use " " as the middle whitespace fragment if the generated one is empty
                   . (#standardMeta % #interstitialWhitespace
                      %~ \whitespaceFragments -> whitespaceFragments
                      & ix (length whitespaceFragments `div` 2 + 1)
                      %~ \whitespaceFragment -> if Text.null whitespaceFragment
                          then " "
                          else whitespaceFragment))
            , addExprMeta 2 (uncurry . Sum)
            , addExprMeta 0 ExprCstrSite
            ]
      where
        addExprMeta minimumWhitespaceFragments c
            = (\meta' parenthesisWhitespace args ->
               c ExprMeta { parenthesisLevels = length parenthesisWhitespace
                          , standardMeta      = meta'
                                & #interstitialWhitespace
                                %~ (\whitespaceFragments -> toWhitespaceFragment
                                        fst
                                        parenthesisWhitespace
                                    ++ whitespaceFragments
                                    ++ toWhitespaceFragment
                                        snd
                                        parenthesisWhitespace)
                          }
                 args) <$> enumerateValidMeta minimumWhitespaceFragments
            <*> accessValid
            <*> accessValid
        toWhitespaceFragment project
            = map (toText . map unWhitespace . project)

instance ValidEnumerable Decl where
    enumerateValid
        = datatype [ addMeta (uncurry . Decl), addMeta DeclCstrSite ]

instance ValidEnumerable WhereClause where
    enumerateValid
        = datatype
            [ (\decls -> WhereClause
                   Meta { interstitialWhitespace = map
                              (toText . map unWhitespace . fst)
                              (toList decls)
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
