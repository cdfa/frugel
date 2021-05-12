{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Node where

import           Data.Has

import           Frugel.Internal.Meta ( ExprMeta(standardMeta) )
import           Frugel.Meta

import           Optics

import           Text.Megaparsec

newtype CstrSite = CstrSite (Seq (Either Char Node))
    deriving ( Eq, Ord, Show )
    deriving newtype ( One, Stream, IsList, Semigroup, Monoid )

data Node
    = IdentifierNode Identifier
    | ExprNode Expr
    | DeclNode Decl
    | WhereNode WhereClause
    deriving ( Eq, Ord, Show )

data Identifier = Identifier Text | IdentifierCstrSite CstrSite
    deriving ( Eq, Ord, Show )

data Expr
    = Variable ExprMeta Identifier
    | Abstraction ExprMeta Identifier Expr
    | Application ExprMeta Expr Expr
    | Sum ExprMeta Expr Expr
    | ExprCstrSite ExprMeta CstrSite
    deriving ( Eq, Ord, Show, Generic, Has ExprMeta )

data Decl
    = Decl { meta :: Meta, name :: Identifier, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Has Meta )

data WhereClause
    = WhereClause Meta (NonEmpty Decl) | WhereCstrSite Meta CstrSite
    deriving ( Eq, Ord, Show, Generic, Has Meta )

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makePrisms ''CstrSite

makePrisms ''Identifier

makePrisms ''Node

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

instance Cons CstrSite CstrSite (Either Char Node) (Either Char Node) where
    _Cons = _CstrSite % _Cons % aside (re _CstrSite)

instance Snoc CstrSite CstrSite (Either Char Node) (Either Char Node) where
    _Snoc = _CstrSite % _Snoc % swapped % aside (re _CstrSite) % swapped

instance AsEmpty CstrSite

instance IsString Identifier where
    fromString = Identifier . toText

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens

instance Default (Traversal' Node CstrSite) where
    def
        = (_IdentifierNode % _IdentifierCstrSite)
        `adjoin` (_ExprNode % _ExprCstrSite % _2)
        `adjoin` (_DeclNode % _DeclCstrSite % _2)
        `adjoin` (_WhereNode % _WhereCstrSite % _2)

instance Default (Getter CstrSite Identifier) where
    def = to IdentifierCstrSite

instance Default (Prism' Node Identifier) where
    def = _IdentifierNode

instance Default (Getter CstrSite Expr) where
    def = to $ ExprCstrSite defaultExprMeta

instance Default (Prism' Node Expr) where
    def = _ExprNode

instance Default (Getter CstrSite Decl) where
    def = to $ DeclCstrSite defaultMeta

instance Default (Prism' Node Decl) where
    def = _DeclNode

instance Default (Getter CstrSite WhereClause) where
    def = to $ WhereCstrSite defaultMeta

instance Default (Prism' Node WhereClause) where
    def = _WhereNode

class (Default (Prism' Node a), Default (Getter CstrSite a)) => IsNode a where
    nodePrism :: Prism' Node a
    nodePrism = def
    fromCstrSite :: Getter CstrSite a
    fromCstrSite = def

instance IsNode Identifier

instance IsNode Expr

instance IsNode Decl

instance IsNode WhereClause