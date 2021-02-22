module ParsingUtils where

import           Text.Megaparsec

-- import           Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L
-- type Lexer = Parsec Void Text
data Parenthesis = Left | Right
    deriving ( Eq, Ord, Show )

pToken :: MonadParsec e s m => Token s -> m (Token s)
pToken c = token (guarded (== c)) (one . Tokens . one $ c)
-- spaceConsumer :: Lexer ()
-- spaceConsumer
--     = L.space
--         space1
--         empty -- No comments for now
--         empty
-- lexeme :: Lexer a -> Lexer a
-- lexeme = L.lexeme spaceConsumer
-- symbol :: Text -> Lexer Text
-- symbol = L.symbol spaceConsumer