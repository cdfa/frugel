{-# LANGUAGE DataKinds #-}
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
import qualified Data.Text        as Text
import           GHC.Exts
import           Prettyprinter
import           Data.Composition

import           Internal.Meta    ( Meta )
import           PrettyPrinting

data Expr
    = Identifier Meta Text
    | Abstraction Meta Text Expr
    | Application Meta Expr Expr
    | Sum Meta Expr Expr
    | ExprHole Meta HoleContents
    deriving ( Eq, Ord, Show )

newtype HoleContents = HoleContents (Seq (Either Char Node))
    deriving ( Eq, Ord, Show, One, Stream, IsList )

data Node = ExprNode Expr | DeclNode Decl | WhereNode WhereClause
    deriving ( Eq, Ord, Show )

data Decl
    = Decl { name :: Text, value :: Expr }
      -- , whereClause :: WhereClause
    | DeclHole HoleContents
    deriving ( Eq, Ord, Show )

data WhereClause = WhereClause [Decl] | WhereHole HoleContents
    deriving ( Eq, Ord, Show )

makeFieldLabelsWith noPrefixFieldLabels ''Decl

instance VisualStream HoleContents where
    showTokens Proxy
        = showTokens (Proxy @String)
        -- Convert from Text to NonEmpty Char
        . fromList -- Assumption: prettyHoleContents of a non-empty Seq results in a non-empty render
        . toList
        -- Remove surrounding «»
        . Text.tail
        . Text.init
        -- Convert from HoleContents to Text
        . renderSmart @Text
        . prettyHoleContents
        -- Convert from NonEmpty to HoleContents
        . fromList
        . toList
    tokensLength = length .: showTokens

exprMeta :: Lens' Expr Meta
exprMeta = lens getMeta setMeta
  where
    getMeta (Identifier meta _) = meta
    getMeta (Abstraction meta _ _) = meta
    getMeta (Application meta _ _) = meta
    getMeta (Sum meta _ _) = meta
    getMeta (ExprHole meta _) = meta
    setMeta (Identifier _ text) meta = Identifier meta text
    setMeta (Abstraction _ argument body) meta = Abstraction meta argument body
    setMeta (Application _ function argument) meta
        = Application meta function argument
    setMeta (Sum _ left right) meta = Application meta left right
    setMeta (ExprHole _ contents) meta = ExprHole meta contents

-- >>> import           Internal.Meta    ( defaultMeta )
-- >>> testPrettyW 3 . prettyExpr . Abstraction defaultMeta "x" $ Sum defaultMeta ( Identifier defaultMeta "x" ) ( Identifier defaultMeta "x" )
-- >>> testPrettyW 8 $ prettyExpr (Application defaultMeta (Application defaultMeta (Identifier defaultMeta "test") $ Identifier defaultMeta "test") $ Identifier defaultMeta "test")
-- \x
--     = x
--     + x
-- test
--     test
--     test
prettyExpr :: Expr -> Doc HoleAnnotation
prettyExpr expr
    | expr ^. exprMeta % #parenthesized
        = parens $ prettyExpr (expr & exprMeta % #parenthesized .~ False)
prettyExpr (Identifier _ n) = pretty n
prettyExpr (Abstraction _ arg expr)
    = (backslash <> pretty arg) `nestingLine` equals <+> prettyExpr expr
prettyExpr (Application _ function arg)
    = prettyExpr function `nestingLine` prettyExpr arg
prettyExpr (Sum _ left right)
    = prettyExpr left `nestingLine` "+" <+> prettyExpr right
prettyExpr (ExprHole _ contents) = prettyHoleContents contents

-- Invariant: prettyHoleContents of a non-empty Seq results in a non-empty render
prettyHoleContents :: HoleContents -> Doc HoleAnnotation
prettyHoleContents (HoleContents contents)
    = if null $ toList contents
        then "..."
        else annotate InHole
            . foldMap
                (either pretty (foldMap (annotate OutOfHole . prettyNode)))
            . groupByEither
            $ toList contents

prettyNode :: Node -> Doc HoleAnnotation
prettyNode (ExprNode expr) = prettyExpr expr
prettyNode (DeclNode decl) = prettyDecl decl
prettyNode (WhereNode w) = prettyWhereClause w

prettyDecl :: Decl -> Doc HoleAnnotation
prettyDecl Decl{..} = pretty name `nestingLine` equals <+> prettyExpr value
prettyDecl (DeclHole contents) = prettyHoleContents contents

    -- <> prettyWhereClause whereClause
prettyWhereClause :: WhereClause -> Doc HoleAnnotation
prettyWhereClause (WhereHole contents) = prettyHoleContents contents
prettyWhereClause (WhereClause decls)
    = if null decls
        then mempty
        else nest
            2
            (line <> "where" <> nest 2 (line <> vcat (map prettyDecl decls)))
