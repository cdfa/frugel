{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Frugel.Internal.Model where

import Frugel.Error

import Optics.Extra

data Model p = Model { cursorOffset :: Int, program :: p, errors :: [Error p] }

makeFieldLabelsWith noPrefixFieldLabels ''Model