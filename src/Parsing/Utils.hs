module Parsing.Utils where

import           Text.Megaparsec
import qualified Data.Set        as Set
import           Prettyprinter
import           Node

type Parser = Parsec Void HoleContents

data Parenthesis = Left | Right
    deriving ( Eq, Ord, Show )

instance Pretty Parenthesis where
    pretty Parsing.Utils.Left = lparen
    pretty Parsing.Utils.Right = rparen

literalToken :: MonadParsec e s m => Token s -> m (Token s)
literalToken c = token (guarded (== c)) (one . Tokens . one $ c)

namedToken :: MonadParsec e s m => String -> (Token s -> Maybe a) -> m a
namedToken name test = token test Set.empty <?> name
