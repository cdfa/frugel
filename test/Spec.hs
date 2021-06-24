{-# LANGUAGE TypeApplications #-}

import Scout

import Test.Syd
import Test.Syd.Validity.GenValidity

main :: IO ()
main = sydTest $ do
    describe "GenValid instance for the AST" $ do
        genValidSpec @Program
        genValidSpec @ProgramMeta
        genValidSpec @Meta
        genValidSpec @CstrSite
        genValidSpec @Expr
        genValidSpec @ExprMeta
        genValidSpec @Identifier
        genValidSpec @WhereClause
        genValidSpec @Decl
