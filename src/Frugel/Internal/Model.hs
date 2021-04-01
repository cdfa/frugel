{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.Model where

import           Frugel.Program

import           Optics

data Model
    = Model { program :: Program, cursorOffset :: Integer, errors :: [String] }
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''Model
