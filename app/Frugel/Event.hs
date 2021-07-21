{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Event where

import Data.Aeson.Types

import Frugel

import Miso         ( Attribute, Options(..), defaultOptions, onWithOptions )
import Miso.Event.Decoder hiding ( keyInfoDecoder )

import Optics.Extra

import Scout.Action

data KeyInfo
    = KeyInfo { key :: !String, shiftKey, metaKey, ctrlKey, altKey :: !Bool }
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''KeyInfo

keyDownHandler :: Attribute Action
keyDownHandler = onKeyDownWithInfo handleKeyDown
  where
    handleKeyDown keyInfo@KeyInfo{..}
        = if noModifiers keyInfo
          then (case key of
                    [c] -> GenericAction $ Insert c
                    "Enter" -> GenericAction $ Insert '\n'
                    "Tab" -> GenericAction $ Insert '\t'
                    "Delete" -> GenericAction Delete
                    "Backspace" -> GenericAction Backspace
                    "ArrowLeft" -> GenericAction $ Move Leftward
                    "ArrowRight" -> GenericAction $ Move Rightward
                    "ArrowUp" -> GenericAction $ Move Upward
                    "ArrowDown" -> GenericAction $ Move Downward
                    _ -> Log key)
          else (case key of
                    [c] | singleModifier #shiftKey keyInfo ->
                            GenericAction $ Insert c
                    "Enter" | singleModifier #ctrlKey keyInfo -> PrettyPrint
                    -- Up and down also available with Alt to prevent window scrolling until https://github.com/dmjio/miso/issues/652 is fixed
                    "ArrowUp" | singleModifier #altKey keyInfo ->
                                  GenericAction $ Move Upward
                    "ArrowDown" | singleModifier #altKey keyInfo ->
                                    GenericAction $ Move Downward
                    -- Left and right also allowed with Alt, because pressing/releasing Alt repeatedly while navigating is annoying
                    "ArrowLeft" | singleModifier #altKey keyInfo ->
                                    GenericAction $ Move Leftward
                    "ArrowRight" | singleModifier #altKey keyInfo ->
                                     GenericAction $ Move Rightward
                    _ -> Log key)

noModifiers :: KeyInfo -> Bool
noModifiers KeyInfo{..} = not $ metaKey || ctrlKey || altKey || shiftKey

singleModifier :: (Is k A_Setter, Is k A_Getter)
    => Optic k is KeyInfo KeyInfo Bool Bool
    -> KeyInfo
    -> Bool
singleModifier modifier keyInfo
    = noModifiers (keyInfo & modifier .~ False) && view modifier keyInfo

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo
    = onWithOptions (Miso.defaultOptions { preventDefault = False })
                    "keydown"
                    keyInfoDecoder

keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder { .. }
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o -> KeyInfo <$> o .: "key"
        <*> o .: "shiftKey"
        <*> o .: "metaKey"
        <*> o .: "ctrlKey"
        <*> o .: "altKey"
