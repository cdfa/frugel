{-# LANGUAGE TypeFamilies #-}

module Lexing where

import           Frugel
import           Text.Megaparsec hiding ( some )
import qualified Data.Set        as Set

type Lexer = Parsec Void HoleContents

pToken :: Either Char Node -> Lexer (Either Char Node)
pToken c = token (guarded (== c)) (one . Tokens . one $ c)

lexHoleContents :: Lexer (Seq LexerToken)
lexHoleContents =
    fmap fromList . some $
    choice
        [ LambdaToken <$ pToken (Left '\\')
        , NodeToken <$> token rightToMaybe Set.empty
        ]
-- instance Stream HoleContents where
--     type Token HoleContents = WithPos LexerToken
--     type Tokens HoleContents = [WithPos LexerToken]
--     tokensToChunk Proxy = id
--     chunkToTokens Proxy = id
--     chunkLength Proxy = length
--     chunkEmpty Proxy = null