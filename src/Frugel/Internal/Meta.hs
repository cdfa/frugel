{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Meta where

import           Data.Char
import           Data.GenValidity
import           Data.Has
import           Data.Validity.Text ()

import           Optics

data ExprMeta = ExprMeta { standardMeta :: Meta, parenthesisLevels :: Int }
    deriving ( Eq, Ord, Show, Generic, Has Meta )

data ProgramMeta
    = ProgramMeta { standardMeta :: Meta, trailingWhitespace :: Text }
    deriving ( Eq, Ord, Show, Generic, Has Meta )

-- Invariant: the number of whitespace fragments should be equal to the number of places in a node where whitespace can exist
newtype Meta = Meta { interstitialWhitespace :: [Text] }
    deriving ( Eq, Ord, Show, Generic )

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
            ]

instance Validity ProgramMeta where
    validate
        = mconcat
            [ genericValidate
            , decorate "trailingWhitespace"
              . checkWhitespace
              . trailingWhitespace
            ]

instance Validity Meta where
    validate
        = mconcat
            [ genericValidate
            , decorate "interstitialWhitespace"
              . flip decorateList checkWhitespace
              . interstitialWhitespace
            ]

checkWhitespace :: ToString a => a -> Validation
checkWhitespace
    = declare "The whitespace fragments contain only whitespace"
    . all isSpace
    . toString
