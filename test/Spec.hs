{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Sized

import EvaluationSpec

import Scout

import Test.Syd
import Test.Syd.Validity.GenValidity

main :: IO ()
main = sydTest $ do
    describe "GenValid instance for the AST" $ do
        modifyMaxSize (const 500) $ genValidSpec @(Sized 500 Program)
        genValidSpec @(Sized 80 Program)
        genValidSpec @ProgramMeta
        genValidSpec @Meta
        genValidSpec @CstrSite
        genValidSpec @Expr
        genValidSpec @ExprMeta
        genValidSpec @Identifier
        genValidSpec @WhereClause
        genValidSpec @Decl
    EvaluationSpec.spec
