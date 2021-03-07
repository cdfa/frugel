{-# LANGUAGE RecordWildCards #-}

module View.Elements where

import           Miso
import qualified Miso.String
import           View.ViewModel

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

inHoleStyles :: HorizontalOpenness -> [Attribute action]
inHoleStyles v
    = [ style_ $ fromList [ ("background-color", "hsl(48, 100%, 85%)") ]
      , paddingStyle v
      ]

inHole
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
inHole v = span . (++ inHoleStyles v)

outOfHoleStyles :: HorizontalOpenness -> [Attribute action]
outOfHoleStyles v = [ class_ "has-background-white", paddingStyle v ]

outOfHole
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
outOfHole v = span . (++ outOfHoleStyles v)

node :: [Attribute action] -> [View action] -> View action
node = span . (class_ "node" :)
