{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module PrettyPrinting.Rendering
    , prettyNode
    , prettyHoleContents
    ) where

import           Miso                     hiding ( node )
import           Text.Megaparsec
import           Node
import           Lexing
import           Parsing
import           PrettyPrinting.Rendering
import           Data.String              as String

-- Type synonym for an application model
type Model = HoleContents

initialModel :: HoleContents
initialModel = frugelId'

-- Sum type for application events
-- data Action = AddOne | SubtractOne | NoOp | SayHelloWorld
data Action = NoOp
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

-- updateModel AddOne m = noEff (m + 1)
-- updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp = noEff

-- updateModel SayHelloWorld m =
--     m <# do liftIO (putStrLn "Hello World") >> pure NoOp
parseHole :: FilePath -> HoleContents -> Either String Node
parseHole filePath s
    = do
        lexerTokens <- runParser'' (holeContents <* eof) s
        runParser'' (expr <* eof) lexerTokens
  where
    runParser'' parser stream
        = first (String.unlines . map parseErrorPretty . toList . bundleErrors)
        $ runParser parser filePath stream
