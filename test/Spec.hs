{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Sized

import Scout

import Test.Syd
import Test.Syd.Validity.GenValidity

main :: IO ()
main = sydTest $ do
    describe "GenValid instance for the AST" $ do
        modifyMaxSize (const 15) . modifyMaxSuccess (const 20)
            $ genValidSpec @(Sized 15 Program)
        genValidSpec @(Sized 80 Program)
        genValidSpec @ProgramMeta
        genValidSpec @Meta
        genValidSpec @CstrSite
        genValidSpec @Expr
        genValidSpec @ExprMeta
        genValidSpec @Identifier
        genValidSpec @WhereClause
        genValidSpec @Decl
