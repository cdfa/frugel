module Frugel where

import           Miso
import qualified ParsingUtils

-- Type synonym for an application model
type Model = Int

-- Sum type for application events
data Action = AddOne | SubtractOne | NoOp | SayHelloWorld
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
    m <# do liftIO (putStrLn "Hello World") >> pure NoOp

data Node =
      Identifier Text
    | Abstraction Text Node
    | Application Node Node
    | Parenthesized Node
    | Hole (Maybe HoleContents)
    deriving ( Eq, Ord, Show )

data LexerToken =
      IdentifierToken Text
    | LambdaToken
    | EqualsToken
    | Parenthesis ParsingUtils.Parenthesis
    | NodeToken Node
    deriving ( Eq, Ord, Show )

type HoleContents = Seq (Either Char Node)
