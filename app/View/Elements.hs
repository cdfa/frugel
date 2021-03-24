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
