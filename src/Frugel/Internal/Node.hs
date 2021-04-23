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

newtype CstrMaterials = CstrMaterials (Seq (Either Char Node))
    deriving ( Eq, Ord, Show )
    deriving newtype ( One, Stream, IsList, Semigroup, Monoid )

data Node
    = IdentifierNode Identifier
    | ExprNode Expr
    | DeclNode Decl
    | WhereNode WhereClause
    deriving ( Eq, Ord, Show )

data Identifier = Identifier Text | IdentifierCstrSite CstrMaterials
    deriving ( Eq, Ord, Show )

data Expr
    = IdentifierExpr ExprMeta Identifier
    | Abstraction ExprMeta Identifier Expr
    | Application ExprMeta Expr Expr
    | Sum ExprMeta Expr Expr
    | ExprCstrSite ExprMeta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has ExprMeta )

data Decl
    = Decl { meta :: Meta, name :: Identifier, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclCstrSite Meta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has Meta )

data WhereClause
    = WhereClause Meta (NonEmpty Decl) | WhereCstrSite Meta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has Meta )

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makePrisms ''CstrMaterials

makePrisms ''Identifier

instance Cons CstrMaterials CstrMaterials (Either Char Node) (Either Char Node) where
    _Cons = _CstrMaterials % _Cons % aside (re _CstrMaterials)

instance Snoc CstrMaterials CstrMaterials (Either Char Node) (Either Char Node) where
    _Snoc
        = _CstrMaterials
        % _Snoc
        % swapped
        % aside (re _CstrMaterials)
        % swapped

instance AsEmpty CstrMaterials

instance IsString Identifier where
    fromString = Identifier . toText

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens
