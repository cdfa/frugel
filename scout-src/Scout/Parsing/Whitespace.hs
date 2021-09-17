{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Scout.Parsing.Whitespace where

import Data.Char
import Data.Has
import qualified Data.Set as Set

import Optics.Extra.Scout

import Scout.Node

import Text.Megaparsec hiding ( some )

type WithWhitespace a = ([Text], a)

whitespaceToken :: (MonadParsec e s m, Token s ~ Either Char Node) => m Text
whitespaceToken = fromMaybe "" <$> optional whitespace'
  where
    whitespace'
        = fmap toText . some . hidden
        $ token (leftToMaybe >=> guarded isSpace) Set.empty

whitespace :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m (WithWhitespace ())
whitespace = (, ()) . one <$> whitespaceToken

noWhitespace :: a -> WithWhitespace a
noWhitespace a = ([], a)

setWhitespace :: forall a. Has Meta a => WithWhitespace a -> a
setWhitespace (whitespaceFragments, a)
    = a
    & (hasLens @Meta @a % #interstitialWhitespace)
    .~ reverse whitespaceFragments

infixl 4 <$%>, <$%, <*%>, <*%, *%>

(<$%>) :: Functor f => (a -> b) -> f a -> f (WithWhitespace b)
(<$%>) f fa = second f . noWhitespace <$> fa

(<$%) :: Functor f => a -> f b -> f (WithWhitespace a)
(<$%) a fb = const a <$%> fb

(<*%>) :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m (WithWhitespace (a -> b))
    -> m a
    -> m (WithWhitespace b)
(<*%>) ff fa
    = (\(whitespaceFragments, f) ws a ->
       (ws : whitespaceFragments, f a)) <$> ff <*> whitespaceToken <*> fa

(<*%) :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m (WithWhitespace a)
    -> m b
    -> m (WithWhitespace a)
(<*%) fa fb = const <<$>> fa <*%> fb

(*%>) :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m a
    -> m (WithWhitespace b)
    -> m (WithWhitespace b)
(*%>) fa fb = (first . flip snoc) <$ fa <*> whitespaceToken <*> fb

wSome :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m a
    -> m (WithWhitespace (NonEmpty a))
wSome fa = bimap reverse fromList <$> wSome'
  where
    wSome'
        = second . cons <$> fa
        <*> (try (first . cons <$> whitespaceToken <*> wSome')
             <|> pure (noWhitespace []))

wMany :: (MonadParsec e s m, Token s ~ Either Char Node)
    => m a
    -> m (WithWhitespace [a])
wMany fa = toList <<$>> wSome fa <|> pure (noWhitespace [])
-- wOptional
--     :: MonadPlus m => m (WithWhitespace a) -> m (WithWhitespace (Maybe a))
-- wOptional fa = Just <<$>> fa <|> pure (noWhitespace Nothing)
