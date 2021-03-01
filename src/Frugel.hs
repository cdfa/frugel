{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module PrettyPrinting.Rendering
    , prettyProgram
    , prettyHoleContents
    ) where

import           Relude                   ( toList )
import           Miso                     hiding ( node )
import           Text.Megaparsec
import           Node
import           Internal.Program         ( Program, prettyProgram )
import           Lexing
import           Parsing
import           PrettyPrinting.Rendering
import           Data.String              as String

-- Type synonym for an application model
type Model = HoleContents

initialModel :: HoleContents
initialModel = Node.sumTest

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
parseHole :: FilePath -> HoleContents -> Either String Program
parseHole filePath s
    = do
        lexerTokens <- first ("Lexer error:\n" ++)
            $ runParser'' (whitespace *> holeContents <* eof) s
        first ("Parser error:\n" ++) $ runParser'' (program <* eof) lexerTokens
  where
    runParser'' parser stream
        = first (String.unlines . map parseErrorPretty . toList . bundleErrors)
        $ runParser parser filePath stream
