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

module Internal.Node where

import           Optics
import           Text.Megaparsec
import qualified Data.Text           as Text
-- import           GHC.Exts
import           Prettyprinter
import           Data.Composition

import           Internal.Meta       ( ExprMeta(standardMeta), Meta )
import           PrettyPrinting.Text
import           Data.Has

newtype CstrMaterials = CstrMaterials (Seq (Either Char Node))
    deriving ( Eq, Ord, Show )
    deriving newtype ( One, Stream, IsList, Semigroup, Monoid )

data Node = ExprNode Expr | DeclNode Decl | WhereNode WhereClause
    deriving ( Eq, Ord, Show )

data Expr
    = Identifier ExprMeta Text
    | Abstraction ExprMeta Text Expr
    | Application ExprMeta Expr Expr
    | Sum ExprMeta Expr Expr
    | ExprCstrSite ExprMeta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has ExprMeta )

data Decl
    = Decl { meta :: Meta, name :: Text, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclCstrSite Meta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has Meta )

data WhereClause = WhereClause Meta [Decl] | WhereCstrSite Meta CstrMaterials
    deriving ( Eq, Ord, Show, Generic, Has Meta )

makeFieldLabelsWith noPrefixFieldLabels ''Decl

makePrisms ''CstrMaterials

instance Cons CstrMaterials CstrMaterials (Either Char Node) (Either Char Node) where
    _Cons = _CstrMaterials % _Cons % aside (re _CstrMaterials)

instance AsEmpty CstrMaterials

instance VisualStream CstrMaterials where
    showTokens Proxy
        = showTokens (Proxy @String)
        -- Convert from Text to NonEmpty Char
        . fromList -- Assumption: prettyCstrMaterials of a non-empty Seq results in a non-empty render
        . toList
        -- Remove surrounding «»
        . Text.tail
        . Text.init
        -- Convert from CstrMaterials to Text
        . renderSmart @Text
        . prettyCstrMaterials
        -- Convert from NonEmpty to CstrMaterials
        . fromFoldable
    tokensLength = length .: showTokens

instance Has Meta Expr where
    getter e = standardMeta $ getter e
    modifier = over (exprMeta % #standardMeta)

exprMeta :: Lens' Expr ExprMeta
exprMeta = hasLens

-- >>> import           Internal.ExprMeta    ( defaultMeta )
-- >>> testPrettyW 3 . prettyExpr . Abstraction defaultMeta "x" $ Sum defaultMeta ( Identifier defaultMeta "x" ) ( Identifier defaultMeta "x" )
-- >>> testPrettyW 8 $ prettyExpr (Application defaultMeta (Application defaultMeta (Identifier defaultMeta "test") $ Identifier defaultMeta "test") $ Identifier defaultMeta "test")
-- \x
--     = x
--     + x
-- test
--     test
--     test
prettyExpr :: Expr -> Doc Annotation
prettyExpr expr
    | (expr ^. exprMeta % #parenthesisLevels) > 0
        = parens $ prettyExpr (expr & exprMeta % #parenthesisLevels -~ 1)
prettyExpr (Identifier _ n) = pretty n
prettyExpr (Abstraction _ arg expr)
    = (backslash <> pretty arg) `nestingLine` equals <+> prettyExpr expr
prettyExpr (Application _ function arg)
    = prettyExpr function `nestingLine` prettyExpr arg
prettyExpr (Sum _ left right)
    = prettyExpr left `nestingLine` "+" <+> prettyExpr right
prettyExpr (ExprCstrSite _ contents) = prettyCstrMaterials contents

-- Invariant: prettyCstrMaterials of a non-empty Seq results in a non-empty render
prettyCstrMaterials :: CstrMaterials -> Doc Annotation
prettyCstrMaterials (CstrMaterials contents)
    = if null $ toList contents
        then "..."
        else annotateInConstruction
            . foldMap (either pretty (foldMap (annotateComplete . prettyNode)))
            . groupByEither
            $ toList contents

prettyNode :: Node -> Doc Annotation
prettyNode (ExprNode expr) = prettyExpr expr
prettyNode (DeclNode decl) = prettyDecl decl
prettyNode (WhereNode w) = prettyWhereClause w

prettyDecl :: Decl -> Doc Annotation
prettyDecl Decl{..} = pretty name `nestingLine` equals <+> prettyExpr value
prettyDecl (DeclCstrSite _ contents) = prettyCstrMaterials contents

    -- <> prettyWhereClause whereClause
prettyWhereClause :: WhereClause -> Doc Annotation
prettyWhereClause (WhereCstrSite _ contents) = prettyCstrMaterials contents
prettyWhereClause (WhereClause _ decls)
    = if null decls
        then mempty
        else nest
            2
            (line <> "where" <> nest 2 (line <> vcat (map prettyDecl decls)))
