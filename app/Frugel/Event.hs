{-# LANGUAGE RecordWildCards #-}

module Frugel.Event where

import           Data.Aeson.Types

import           Frugel

import           Miso
                 ( Attribute, Options(..), defaultOptions, onWithOptions )
import           Miso.Event.Decoder hiding ( keyInfoDecoder )

data KeyInfo
    = KeyInfo { key :: !String, shiftKey, metaKey, ctrlKey, altKey :: !Bool }
    deriving ( Show, Eq )

keyDownHandler :: Attribute Action
keyDownHandler = onKeyDownWithInfo handleKeyDown
  where
    handleKeyDown keyInfo@KeyInfo{..}
        = if noModifiers keyInfo
            then (case key of
                      [c] -> Insert c
                      "Enter" -> Insert '\n'
                      "Tab" -> Insert '\t'
                      "ArrowLeft" -> Move Leftward
                      "ArrowRight" -> Move Rightward
                      "ArrowUp" -> Move Upward
                      "ArrowDown" -> Move Downward
                      _ -> Log key)
            else (case key of
                      "Enter"
                          | not metaKey
                              && ctrlKey
                              && not altKey
                              && not shiftKey -> PrettyPrint
                      -- Up and down also available with Alt to prevent window scrolling until https://github.com/dmjio/miso/issues/652 is fixed
                      "ArrowUp"
                          | not metaKey
                              && not ctrlKey
                              && altKey
                              && not shiftKey -> Move Upward
                      "ArrowDown"
                          | not metaKey
                              && not ctrlKey
                              && altKey
                              && not shiftKey -> Move Downward
                      _ -> Log key)

noModifiers :: KeyInfo -> Bool
noModifiers KeyInfo{..} = not $ metaKey || ctrlKey || altKey

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo
    = onWithOptions
        (Miso.defaultOptions { preventDefault = False })
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
