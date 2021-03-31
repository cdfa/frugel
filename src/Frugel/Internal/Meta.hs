{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Meta where

import           Data.Has

import           Optics

data ExprMeta = ExprMeta { standardMeta :: Meta, parenthesisLevels :: Int }
    deriving ( Eq, Ord, Show, Generic, Has Meta )

data ProgramMeta
    = ProgramMeta { standardMeta :: Meta, trailingWhitespace :: Text }
    deriving ( Eq, Ord, Show, Generic, Has Meta )

newtype Meta = Meta { interstitialWhitespace :: [Text] }
    deriving ( Eq, Ord, Show )

makeFieldLabelsWith noPrefixFieldLabels ''ExprMeta

makeFieldLabelsWith noPrefixFieldLabels ''ProgramMeta

makeFieldLabelsWith noPrefixFieldLabels ''Meta

defaultExprMeta :: ExprMeta
defaultExprMeta
    = ExprMeta { parenthesisLevels = 0, standardMeta = defaultMeta }

defaultProgramMeta :: ProgramMeta
defaultProgramMeta
    = ProgramMeta { standardMeta = defaultMeta, trailingWhitespace = "" }

defaultMeta :: Meta
defaultMeta = Meta { interstitialWhitespace = [] }
