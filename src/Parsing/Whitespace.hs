{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Parsing.Whitespace where

import           Data.Has
import qualified Data.Set        as Set
import           Internal.Meta   ( Meta )
import           Optics
import           Text.Megaparsec
import           Lexing

type WithWhitespace a = ([Text], a)

whitespaceToken :: (MonadParsec e s m, Token s ~ LexerToken) => m Text
whitespaceToken
    = fromMaybe "" <$> optional (token (preview _WhitespaceToken) Set.empty)

whitespace :: (MonadParsec e s m, Token s ~ LexerToken)
    => m (WithWhitespace ())
whitespace = (, ()) . one <$> whitespaceToken

noWhitespace :: a -> WithWhitespace a
noWhitespace a = ([], a)

setWhitespace :: forall a. Has Meta a => WithWhitespace a -> a
setWhitespace (whitespaceFragments, a)
    = a
    & (hasLens @Meta @a % #interstitialWhitespace)
    .~ reverse whitespaceFragments

infixl 4 <$%>, <$%, <*%>, <*%

(<$%>) :: Functor f => (a -> b) -> f a -> f (WithWhitespace b)
(<$%>) f fa = second f . noWhitespace <$> fa

(<$%) :: Functor f => a -> f b -> f (WithWhitespace a)
(<$%) a fb = const a <$%> fb

(<*%>) :: (MonadParsec e s m, Token s ~ LexerToken)
    => m (WithWhitespace (a -> b))
    -> m a
    -> m (WithWhitespace b)
(<*%>) ff fa
    = (\(whitespaceFragments, f) ws a -> (ws : whitespaceFragments, f a))
    <$> ff
    <*> whitespaceToken
    <*> fa

(<*%) :: (MonadParsec e s m, Token s ~ LexerToken)
    => m (WithWhitespace a)
    -> m b
    -> m (WithWhitespace a)
(<*%) fa fb = const <<$>> fa <*%> fb

(*%>) :: (MonadParsec e s m, Token s ~ LexerToken)
    => m a
    -> m (WithWhitespace b)
    -> m (WithWhitespace b)
(*%>) fa fb = (first . (:)) <$ fa <*> whitespaceToken <*> fb

wSome :: (MonadParsec e s m, Token s ~ LexerToken)
    => m a
    -> m (WithWhitespace [a])
wSome fa
    = (\a ws (wss, as) -> (ws : wss, a : as)) <$> fa
    <*> whitespaceToken
    <*> wMany fa

wMany :: (MonadParsec e s m, Token s ~ LexerToken)
    => m a
    -> m (WithWhitespace [a])
wMany fa = wSome fa <|> pure (noWhitespace [])

wOptional
    :: MonadPlus m => m (WithWhitespace a) -> m (WithWhitespace (Maybe a))
wOptional fa = Just <<$>> fa <|> pure (noWhitespace Nothing)
