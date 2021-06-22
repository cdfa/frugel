{-# LANGUAGE FlexibleContexts #-}

module Scout.Meta
    ( module Scout.Meta
    , Meta(Meta)
    , ExprMeta(ExprMeta)
    , ProgramMeta(ProgramMeta)
    , enumerateValidExprMeta
    , enumerateValidProgramMeta
    , enumerateValidMeta
    ) where

import Data.GenValidity
import Data.Has
import Data.Validity.Text ()

import Scout.Internal.Meta

defaultExprMeta :: Int -> ExprMeta
defaultExprMeta n
    = ExprMeta { parenthesisLevels = 0, standardMeta = defaultMeta n }

defaultProgramMeta :: Int -> ProgramMeta
defaultProgramMeta n
    = ProgramMeta { standardMeta = defaultMeta n, trailingWhitespace = "" }

defaultMeta :: Int -> Meta
defaultMeta n = Meta { interstitialWhitespace = replicate n "" }

validateInterstitialWhitespace :: Has Meta b => (b -> Int) -> b -> Validation
validateInterstitialWhitespace expectedWhitespaceFragmentCount n
    = mconcat [ genericValidate
              , declare "has the correct number of whitespace fragments"
                . (== expectedWhitespaceFragmentCount n)
                . length
                . interstitialWhitespace
              ]
    $ getter n
