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
        [ Identifier <$> identifier
        , Abstraction <$ pToken LambdaToken <*> identifier
          <* pToken EqualsToken
          <*> node
        , Parenthesized <$ pToken (Parenthesis ParsingUtils.Left) <*> node
          <* pToken (Parenthesis ParsingUtils.Right)
        , token nodeTokenToNode Set.empty <?> "a node"
        , Application <$> node <*> node -- Moved to bottom because of left recursion
        ]
