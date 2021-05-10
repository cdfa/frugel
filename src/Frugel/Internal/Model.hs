{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Model where

import           Frugel.Error
import           Frugel.Program

import           Optics

data Model
    = Model { cursorOffset :: Int, program :: Program, errors :: [Error] }
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''Model