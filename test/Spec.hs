{-# LANGUAGE TypeApplications #-}

import Frugel.Program

import Test.Syd
import Test.Syd.Validity.GenValidity

main :: IO ()
main = sydTest $ do
    describe "GenValid instance for the AST" $ do
        genValidSpec @Program
