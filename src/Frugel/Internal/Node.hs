{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Node where

import           Control.Enumerable.Combinators     ()
import           Control.ValidEnumerable
import           Control.ValidEnumerable.Whitespace

import           Data.Data
import           Data.GenValidity
import           Data.GenValidity.Sequence          ()
import           Data.Has
import qualified Data.Text                          as Text
import           Data.Validity.Sequence             ()

import           Frugel.Identifier
import           Frugel.Internal.Meta               ( ExprMeta(standardMeta) )
import qualified Frugel.Internal.Meta
import           Frugel.Meta

import           Optics

import           Relude.Unsafe                      ( (!!) )

import           Test.QuickCheck.Gen                as Gen

import           Text.Megaparsec.Stream

newtype CstrSite = CstrSite (Seq (Either Char Node))
    deriving ( Eq, Ord, Show, Generic, Data )
    deriving newtype ( One, Stream, IsList, Semigroup, Monoid )

data Node = ExprNode Expr | DeclNode Decl | WhereNode WhereClause
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

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makePrisms ''CstrSite

makePrisms ''Node

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

instance Cons CstrSite CstrSite (Either Char Node) (Either Char Node) where
    _Cons = _CstrSite % _Cons % aside (re _CstrSite)

instance Snoc CstrSite CstrSite (Either Char Node) (Either Char Node) where
    _Snoc = _CstrSite % _Snoc % swapped % aside (re _CstrSite) % swapped

instance AsEmpty CstrSite

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

instance Default (AffineTraversal' Node CstrSite) where
    def
        = singular
        $ (_ExprNode % _ExprCstrSite % _2)
        `adjoin` (_DeclNode % _DeclCstrSite % _2)
        `adjoin` (_WhereNode % _WhereCstrSite % _2)

instance Default (Getter CstrSite Expr) where
    def = to exprCstrSite'

instance Default (Prism' Node Expr) where
    def = _ExprNode

instance Default (Getter CstrSite Decl) where
    def = to declCstrSite'

instance Default (Prism' Node Decl) where
    def = _DeclNode

instance Default (Getter CstrSite WhereClause) where
    def = to whereCstrSite'

instance Default (Prism' Node WhereClause) where
    def = _WhereNode

class (Default (Prism' Node a), Default (Getter CstrSite a)) => IsNode a where
    nodePrism :: Prism' Node a
    nodePrism = def
    fromCstrSite :: Getter CstrSite a
    fromCstrSite = def

instance IsNode Expr

instance IsNode Decl

instance IsNode WhereClause

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

instance Validity CstrSite

instance Validity Node

instance Validity Expr where
    validate
        = mconcat
            [ genericValidate
            , validateInterstitialWhitespace validInterstitialWhitespace
            , declare "has non-empty middle whitespace"
              . fromMaybe True
              . preview
                  (_Application
                   % _1
                   % #standardMeta
                   % #interstitialWhitespace
                   % to
                       (not
                        . Text.null
                        . (\whitespaceFragments -> whitespaceFragments
                           !! ((length whitespaceFragments `div` 2) + 1))))
            ]

instance Validity Decl where
    validate
        = mconcat
            [ genericValidate
            , validateInterstitialWhitespace validInterstitialWhitespace
            ]

instance Validity WhereClause where
    validate
        = mconcat
            [ genericValidate
            , validateInterstitialWhitespace validInterstitialWhitespace
            ]

instance GenValid CstrSite where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Node where
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

instance ValidEnumerable CstrSite where
    enumerateValid = datatype [ c1 CstrSite ]

instance ValidEnumerable Node where
    enumerateValid = datatype [ c1 ExprNode, c1 DeclNode, c1 WhereNode ]

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
                      & ix ((length whitespaceFragments `div` 2) + 1)
                      %~ \whitespaceFragment -> if Text.null whitespaceFragment
                          then " "
                          else whitespaceFragment))
            , addExprMeta 2 (uncurry . Sum)
            , addExprMeta 0 ExprCstrSite
            ]
      where
        addExprMeta minimumWhitespaceFragments c
            = (\meta' parenthesisWhitespace args -> c
                   ExprMeta { parenthesisLevels = length parenthesisWhitespace
                            , standardMeta      = meta'
                                  & #interstitialWhitespace
                                  %~ (\whitespaceFragments ->
                                      toWhitespaceFragment
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
