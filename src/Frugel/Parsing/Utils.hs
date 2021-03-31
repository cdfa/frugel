module Frugel.Parsing.Utils where

import qualified Data.Set        as Set

import           Frugel.Node

import           Prettyprinter

import           Text.Megaparsec

type Parser = Parsec Void CstrMaterials

data Parenthesis = Left | Right
    deriving ( Eq, Ord, Show )

instance Pretty Parenthesis where
    pretty Frugel.Parsing.Utils.Left = lparen
    pretty Frugel.Parsing.Utils.Right = rparen

literalToken :: MonadParsec e s m => Token s -> m (Token s)
literalToken c = token (guarded (== c)) (one . Tokens . one $ c)

namedToken :: MonadParsec e s m => String -> (Token s -> Maybe a) -> m a
namedToken name test = token test Set.empty <?> name
