{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DerivingStrategies #-}
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

newtype CstrSite = CstrSite (Seq (Either Char NodeItem))
    deriving ( Eq, Ord, Show )
    deriving newtype ( One, Stream, IsList, Semigroup, Monoid )

data NodeItem
    = IdentifierNode Identifier
    | ExprNode Expr
    | DeclNode Decl
    | WhereNode WhereClause
    deriving ( Eq, Ord, Show )

data Identifier = Identifier Text | IdentifierCstrSite CstrSite
    deriving ( Eq, Ord, Show )

data Expr
    = IdentifierExpr ExprMeta Identifier
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

instance Cons CstrSite CstrSite (Either Char NodeItem) (Either Char NodeItem) where
    _Cons = _CstrSite % _Cons % aside (re _CstrSite)

instance Snoc CstrSite CstrSite (Either Char NodeItem) (Either Char NodeItem) where
    _Snoc = _CstrSite % _Snoc % swapped % aside (re _CstrSite) % swapped

instance AsEmpty CstrSite

instance IsString Identifier where
    fromString = Identifier . toText

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens
