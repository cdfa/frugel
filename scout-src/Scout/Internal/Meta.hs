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

import Control.Sized
import Control.ValidEnumerable

import Data.Composition
import Data.Data
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Has
import qualified Data.Text as Text
import Data.Validity.Extra
import Data.Validity.Text ()
import Data.Whitespace

import Optics.Extra.Scout

import qualified Relude.Unsafe as Unsafe

import qualified Test.QuickCheck.Gen as QuickCheck

data ExprMeta = ExprMeta { standardMeta :: Meta, parenthesisLevels :: Int }
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

data ProgramMeta
    = ProgramMeta { standardMeta :: Meta, trailingWhitespace :: Text }
    deriving ( Eq, Ord, Show, Generic, Data, Has Meta )

-- It would be nicer if a heterogeneous type list would be used instead, especially since elided and focused are not used by core Frugel
data Meta
    = Meta { interstitialWhitespace :: [Text] -- Invariant: the number of whitespace fragments should be equal to the number of places in a node where whitespace can exist
             -- If elided == True, the node with this metadata may not have been processed by a previous operation and a dummy result should be used
             -- The various node type should really be made parametric using hypertypes (https://github.com/lamdu/hypertypes), but there are higher priority tasks atm.
             -- ATM this is only set to true by evaluation and obeyed by pretty printing (not standard rendering)
           , elided :: Bool
             -- This is not the source of truth for the cursor location (that's in Model). This is used in evaluation to check if the node is focused
           , focused :: Bool
           }
    deriving ( Eq, Ord, Show, Generic, Data )

makeFieldLabelsWith noPrefixFieldLabels ''ExprMeta

makeFieldLabelsWith noPrefixFieldLabels ''ProgramMeta

makeFieldLabelsWith noPrefixFieldLabels ''Meta

instance Validity ExprMeta where
    validate
        = mconcat
            [ genericValidate
            , decorate
                  "The number of surrounding parentheses (parenthesisLevels)"
              . declare "is greater than or equal to 0"
              . (>= 0)
              . parenthesisLevels
            , decorate "The standardMeta"
              . declare "The number of whitespace fragments is greater or equal then twice the number of surrounding parentheses (parenthesisLevels * 2)"
              . \ExprMeta{..} -> length (interstitialWhitespace standardMeta)
              >= parenthesisLevels
            ]

instance Validity ProgramMeta where
    validate
        = mconcat [ genericValidate
                  , decorate "The trailing whitespace"
                    . validateWhitespace
                    . trailingWhitespace
                  ]

instance Validity Meta where
    validate
        = mconcat [ genericValidate
                  , decorate "The interstitial whitespace"
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
        $ interstitialWhitespace Unsafe.!! i

enumerateValidExprMeta :: (Typeable f, Sized f) => Int -> Shareable f ExprMeta
enumerateValidExprMeta minimumWhitespaceFragments
    = pay
    $ (\meta' parenthesisWhitespace ->
       ExprMeta { parenthesisLevels = length parenthesisWhitespace
                , standardMeta = meta'
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
    = pay $ ProgramMeta <$> enumerateValidMeta n <*> enumerateWhitespace

enumerateValidMeta :: (Typeable f, Sized f) => Int -> Shareable f Meta
enumerateValidMeta n
    = pay
    $ Meta <$> vectorOf n enumerateWhitespace <*> pure False <*> pure False
