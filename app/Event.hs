{-# LANGUAGE RecordWildCards #-}

module Event where

import           Miso               ( Attribute, on )
import           Frugel
import           Miso.Event.Decoder
import           Data.Aeson.Types

data KeyInfo
    = KeyInfo { key :: !String, shiftKey, metaKey, ctrlKey, altKey :: !Bool }
    deriving ( Show, Eq )

keyDownHandler :: Attribute Action
keyDownHandler = onKeyDownWithInfo handleKeyDown
  where
    handleKeyDown KeyInfo{..} = case key of
        [c]
            | not $ metaKey || ctrlKey || altKey -> Insert c
        _ -> Log key

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo = Miso.on "keydown" Event.keyInfoDecoder

keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder { .. }
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o -> KeyInfo <$> o .: "key"
        <*> o .: "shiftKey"
        <*> o .: "metaKey"
        <*> o .: "ctrlKey"
        <*> o .: "altKey"
