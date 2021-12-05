{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Scout.Truncatable where

import Data.Data.Lens

import Optics

import Scout.Node

class Truncatable a where
    truncate :: Int -> a -> a

instance Truncatable (ACstrSite Node) where
    truncate depth = _CstrSite % mapped % _Right %~ truncate (pred depth)

instance Truncatable Node where
    truncate depth n = case n of
        ExprNode expr -> ExprNode $ truncate depth expr
        DeclNode decl -> DeclNode $ truncate depth decl
        WhereNode whereClause -> WhereNode $ truncate depth whereClause

instance Truncatable Expr where
    truncate depth e
        | depth <= 0 = elideExpr e -- use evaluation status for expr, because inspecting it's meta forces the constructor
    truncate depth (ExprCstrSite meta cstrSite)
        = ExprCstrSite meta $ truncate depth cstrSite
    truncate depth expr@IfExpression{}
        = expr & traversalVL uniplate %~ truncate (depth - 4)
    truncate depth expr = expr & traversalVL uniplate %~ truncate (pred depth)

instance Truncatable Decl where
    truncate depth decl | depth <= 0 = elide decl
    truncate depth (DeclCstrSite meta cstrSite)
        = DeclCstrSite meta $ truncate depth cstrSite
    truncate depth decl = decl & #value %~ truncate (pred depth)

instance Truncatable WhereClause where
    truncate depth whereClause | depth <= 0 = elide whereClause
    truncate depth (WhereCstrSite meta cstrSite)
        = WhereCstrSite meta $ truncate depth cstrSite
    truncate depth whereClause
        = whereClause & _WhereClause % _2 % mapped %~ truncate (pred depth)
