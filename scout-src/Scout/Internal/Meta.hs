{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.Meta where

import Control.ValidEnumerable
import Control.ValidEnumerable.Whitespace

import Data.Composition
import Data.Data
import Data.GenValidity
import Data.GenValidity.Text        ()
import Data.Has
import qualified Data.Text          as Text
import Data.Validity.Extra
import Data.Validity.Text           ()

import Optics.Extra

import Relude.Unsafe                ( (!!) )

import qualified Test.QuickCheck.Gen as QuickCheck

data ExprMeta = ExprMeta { standardMeta :: Meta, parenthesisLevels :: Int }
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

data ProgramMeta
    = ProgramMeta { standardMeta :: Meta, trailingWhitespace :: Text }
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

-- Invariant: the number of whitespace fragments should be equal to the number of places in a node where whitespace can exist
newtype Meta = Meta { interstitialWhitespace :: [Text] }
    deriving ( Eq, Ord, Show, Generic, Data )

makeFieldLabelsWith noPrefixFieldLabels ''ExprMeta

makeFieldLabelsWith noPrefixFieldLabels ''ProgramMeta

makeFieldLabelsWith noPrefixFieldLabels ''Meta

instance Validity ExprMeta where
    validate
        = mconcat
            [ genericValidate
            , decorate "parenthesisLevels"
              . declare "the number is greater than or equal to 0"
              . (>= 0)
              . parenthesisLevels
            , decorate "standardMeta"
              . declare "the number of whitespace fragments is greater or equal then parenthesisLevels * 2"
              . \ExprMeta{..} -> length (interstitialWhitespace standardMeta)
              >= parenthesisLevels
            ]

instance Validity ProgramMeta where
    validate
        = mconcat [ genericValidate
                  , decorate "trailingWhitespace"
                    . validateWhitespace
                    --   . traceShowId
                    . trailingWhitespace
                  ]

instance Validity Meta where
    validate
        = mconcat [ genericValidate
                  , decorate "interstitialWhitespace"
                    . flip decorateList validateWhitespace
                    . interstitialWhitespace
                  ]

instance GenValid ExprMeta where
    genValid = QuickCheck.sized . uniformWith $ enumerateValidExprMeta 0
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required since shrinking Ints does not shrink to negative numbers

instance GenValid ProgramMeta where
    genValid = QuickCheck.sized . uniformWith $ enumerateValidProgramMeta 0
    shrinkValid programMeta@ProgramMeta{trailingWhitespace}
        = shrinkValidStructurallyWithoutExtraFiltering programMeta
        & mapped % #trailingWhitespace %~ \whitespaceFragment ->
        Text.take (Text.length whitespaceFragment) trailingWhitespace

instance GenValid Meta where
    genValid = QuickCheck.sized . uniformWith $ enumerateValidMeta 0
    shrinkValid meta@Meta{interstitialWhitespace}
        = shrinkValidStructurallyWithoutExtraFiltering meta
        -- ensure number of whitespace fragments is preserved
        & filter ((length interstitialWhitespace ==)
                  . lengthOf #interstitialWhitespace)
        -- ensure only characters from previous whitespace fragments are used
        & mapped % #interstitialWhitespace % imapped
        %@~ \i whitespaceFragment -> Text.take (Text.length whitespaceFragment)
        $ interstitialWhitespace !! i

enumerateValidExprMeta :: (Typeable f, Sized f) => Int -> Shareable f ExprMeta
enumerateValidExprMeta minimumWhitespaceFragments
    = pay
    $ (\meta' parenthesisWhitespace ->
       ExprMeta { parenthesisLevels = length parenthesisWhitespace
                , standardMeta      = meta'
                      & #interstitialWhitespace
                      %~ (\whitespaceFragments -> map fst parenthesisWhitespace
                          ++ whitespaceFragments
                          ++ map snd parenthesisWhitespace)
                }) <$> enumerateValidMeta minimumWhitespaceFragments
    <*> inflation (2 ^)
                  []
                  ((:) .: (,) <$> enumerateWhitespace <*> enumerateWhitespace)

enumerateValidProgramMeta
    :: (Typeable f, Sized f) => Int -> Shareable f ProgramMeta
enumerateValidProgramMeta n
    = pay
    $ aconcat [ ProgramMeta <$> enumerateValidMeta n <*> enumerateWhitespace ]

enumerateValidMeta :: (Typeable f, Sized f) => Int -> Shareable f Meta
enumerateValidMeta n
    = pay $ aconcat [ Meta <$> vectorOf n enumerateWhitespace ]
