{-# LANGUAGE RecordWildCards #-}

module Frugel.Event where

import           Data.Aeson.Types

import           Frugel

import           Miso               ( Attribute, on )
import           Miso.Event.Decoder hiding ( keyInfoDecoder )

data KeyInfo
    = KeyInfo { key :: !String, shiftKey, metaKey, ctrlKey, altKey :: !Bool }
    deriving ( Show, Eq )

keyDownHandler :: Attribute Action
keyDownHandler = onKeyDownWithInfo handleKeyDown
  where
    handleKeyDown KeyInfo{..} = case key of
        [c]
            | not $ metaKey || ctrlKey || altKey -> Insert c
        "Enter"
            | not $ metaKey || ctrlKey || altKey -> Insert '\n'
        "Tab"
            | not $ metaKey || ctrlKey || altKey -> Insert '\t'
        "Enter"
            | not metaKey && ctrlKey && not altKey && not shiftKey ->
                PrettyPrint
        _ -> Log key

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo = Miso.on "keydown" keyInfoDecoder

keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder { .. }
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o -> KeyInfo <$> o .: "key"
        <*> o .: "shiftKey"
        <*> o .: "metaKey"
        <*> o .: "ctrlKey"
        <*> o .: "altKey"
