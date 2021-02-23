module Parsing where

import           Node
import           Lexing
import           ParsingUtils    hiding ( Left, Right )
import qualified ParsingUtils    ( Parenthesis(..) )
import           Text.Megaparsec
import qualified Data.Set        as Set

type Parser = Parsec Void (Seq LexerToken)

identifier :: Parser Text
identifier = token identifierTokenToText Set.empty <?> "an identifier"

node :: Parser Node
node
    = choice
        [ Abstraction <$ pToken LambdaToken <*> identifier
          <* pToken EqualsToken
          <*> node'
        , Parenthesized <$ pToken (Parenthesis ParsingUtils.Left) <*> node'
          <* pToken (Parenthesis ParsingUtils.Right)
          -- Non recursive production rules at the bottom
        , token nodeTokenToNode Set.empty <?> "a node"
        , Identifier <$> identifier
        ]

-- Extracted to remove left recursion
node' :: Parser Node
node'
    = do
        function <- node
        argMaybe <- optional node'
        case argMaybe of
            -- make application left-associative
            Just (Application argFunction argArg) -> pure
                $ Application (Application function argFunction) argArg
            Just arg -> pure $ Application function arg
            Nothing -> pure function
