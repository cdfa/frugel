{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Meta where

import           Optics

newtype Meta = Meta { parenthesized :: Bool }
    deriving ( Eq, Ord, Show )

makeFieldLabelsWith noPrefixFieldLabels ''Meta

defaultMeta :: Meta
defaultMeta = Meta { parenthesized = False }