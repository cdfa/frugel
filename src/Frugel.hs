{-# LANGUAGE FlexibleContexts #-}

module Frugel where

import           Miso               hiding ( node )
import           Text.Megaparsec
import           Text.Pretty.Simple ( pShowNoColor )
import qualified Data.Text.Lazy     as LazyText
import           Node
import           Lexing
import           Parsing

-- Type synonym for an application model
type Model = HoleContents

initialModel :: HoleContents
initialModel = app

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
parseHole :: FilePath -> HoleContents -> Either Text Node
parseHole filePath s
    = do
        lexerTokens <- runParser'' (holeContents <* eof) s
        runParser'' (expr <* eof) lexerTokens
  where
    runParser'' parser stream
        = first (LazyText.toStrict . pShowNoColor)
        $ runParser parser filePath stream
