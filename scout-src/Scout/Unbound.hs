{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Scout.Unbound where

import Data.Data.Lens
import qualified Data.Set as Set

import Optics

import qualified Scout.Internal.Node
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program

class Unbound a where
    freeVariables :: Set Identifier -> a -> Set Identifier

instance Unbound (ACstrSite Node) where
    freeVariables env cstrSite
        = foldMapOf (_CstrSite % folded % _Right) freeVariables' cstrSite
      where
        freeVariables' = \case
            ExprNode e -> freeVariables newEnv e
            DeclNode d -> freeVariables newEnv d
            WhereNode w -> freeVariables newEnv w
        newEnv
            = mappend env
            . flip (foldMapOf (_CstrSite % folded % _Right)) cstrSite
            $ \case
                ExprNode _ -> mempty
                DeclNode d -> foldMapOf #name Set.singleton d
                WhereNode w -> fromList $ whereClauseBindees w

instance Unbound Expr where
    freeVariables env = \case
        Variable _ n -> if Set.member n env then mempty else Set.singleton n
        Abstraction _ n e -> freeVariables (Set.insert n env) e
        ExprCstrSite _ cstrSite -> freeVariables env cstrSite
        expr -> foldMapOf (traversalVL uniplate) (freeVariables env) expr

instance Unbound Decl where
    freeVariables env Decl{..} = freeVariables (Set.insert name env) value
    freeVariables env (DeclCstrSite _ cstrSite) = freeVariables env cstrSite

instance Unbound WhereClause where
    freeVariables env w@(WhereClause _ decls)
        = foldMap (freeVariables $ env <> fromList (whereClauseBindees w))
                  decls
    freeVariables env (WhereCstrSite _ cstrSite) = freeVariables env cstrSite

instance Unbound Program where
    freeVariables env Program{..}
        = freeVariables newEnv expr <> foldMap (freeVariables env) whereClause
      where
        newEnv
            = env
            <> fromList (whereClause ^.. _Just
                         % ((_WhereClause % _2 % folded % #name)
                            `summing` (_WhereCstrSite
                                       % _2
                                       % _CstrSite
                                       % folded
                                       % _Right
                                       % ((_DeclNode % #name)
                                          `summing` (_WhereNode
                                                     % _WhereClause
                                                     % _2
                                                     % folded
                                                     % #name)))))
    freeVariables env (ProgramCstrSite _ cstrSite) = freeVariables env cstrSite
