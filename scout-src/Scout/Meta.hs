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
import qualified Data.Text as Text
import Data.Validity.Text ()

import Scout.Internal.Meta

defaultExprMeta :: Int -> ExprMeta
defaultExprMeta n
    = ExprMeta { parenthesisLevels = 0, standardMeta = defaultMeta n }

defaultProgramMeta :: Int -> ProgramMeta
defaultProgramMeta n
    = ProgramMeta { standardMeta = defaultMeta n, trailingWhitespace = "" }

defaultMeta :: Int -> Meta
defaultMeta n
    = Meta { interstitialWhitespace = replicate n "", elided = False }

validateInterstitialWhitespace :: Has Meta a => (a -> Int) -> a -> Validation
validateInterstitialWhitespace expectedWhitespaceFragmentCount n
    = mconcat [ genericValidate
              , declare "has the correct number of whitespace fragments"
                . (== expectedWhitespaceFragmentCount n)
                . length
                . interstitialWhitespace
              ]
    $ getter n

hasNonEmptyInterstitialWhitespace :: Has Meta a => a -> Validation
hasNonEmptyInterstitialWhitespace
    = validateInterstitialWhitespaceWith
        (declare "is not empty" . not . Text.null)

validateInterstitialWhitespaceWith
    :: Has Meta b => (Text -> Validation) -> b -> Validation
validateInterstitialWhitespaceWith validateWhitespace
    = decorate "Meta"
    . decorate "The interstitial whitespace"
    . flip decorateList validateWhitespace
    . interstitialWhitespace
    . getter
