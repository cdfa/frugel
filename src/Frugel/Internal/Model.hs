{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.Internal.Model where

import           Frugel.PrettyPrinting
import           Frugel.Program

import           Optics

data Model
    = Model { program      :: Program
            , cursorOffset :: Integer
            , errors       :: [Doc Annotation]
            }
    deriving ( Show )

makeFieldLabelsWith noPrefixFieldLabels ''Model

instance Eq ann => Eq (Doc ann) where
    (==)
        = (==)
        `on` layoutPretty (LayoutOptions { layoutPageWidth = Unbounded })

instance Eq Model where
    (Model program1 cursorOffset1 errors1)
        == (Model program2 cursorOffset2 errors2)
        = program1 == program2
        && cursorOffset1 == cursorOffset2
        && errors1 == errors2
