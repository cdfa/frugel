{-# LANGUAGE RecordWildCards #-}

module Frugel.View.Elements where

import           Frugel.View.ViewModel

import           Miso
import qualified Miso.String

noButtonStyle :: Attribute action
noButtonStyle
    = style_
    $ fromList
        [ ("background", "none")
        , ("color", "inherit")
        , ("border", "none")
        , ("padding", "0")
        , ("font", "inherit")
        , ("cursor", "auto")
        , ("outline", "inherit")
        , ("user-select", "text")
        , ("text-align", "left")
        ]

codeStyle :: Attribute action
codeStyle
    = style_
    $ fromList
        [ ("white-space", "pre")
        , ("font-family", "\"Courier New\", monospace")
        ]

spanStyle :: Attribute action
spanStyle = style_ $ fromList [ ("display", "inline-block") ]

span :: [Attribute action] -> [View action] -> View action
span = span_ . (spanStyle :)

paddingStyle :: HorizontalOpenness -> Attribute action
paddingStyle HorizontalOpenness{..}
    = style_
    $ fromList
        [ ( "padding"
          , Miso.String.ms
            $ unwords [ "4px", padding openRight, "4px", padding openLeft ]
          )
        ]
  where
    padding present = if present then "0px" else "4px"

inConstructionStyles :: HorizontalOpenness -> [Attribute action]
inConstructionStyles v
    = [ style_ $ fromList [ ("background-color", "hsl(48, 100%, 85%)") ]
      , paddingStyle v
      ]

inConstruction
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
inConstruction v = span . (++ inConstructionStyles v)

completeStyles :: HorizontalOpenness -> [Attribute action]
completeStyles v = [ class_ "has-background-white", paddingStyle v ]

complete
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
complete v = span . (++ completeStyles v)

node :: [Attribute action] -> [View action] -> View action
node = span . (class_ "node" :)

cursorStyle :: Attribute action
cursorStyle
    = style_
    $ fromList
        [ ("padding", "0px 1px")
        , ("margin", "0px -1px")
        , ("height", "15px")
        , ("background-color", "black")
        ]-- todo proper height and vcenter

caret :: [Attribute action] -> [View action] -> View action
caret = span . (cursorStyle :)
