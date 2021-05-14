{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Enumerable.Combinators where

import           Control.Enumerable

import           Data.List          ( genericIndex )

vectorOf :: Applicative f => Int -> Shareable f a -> Shareable f [a]
vectorOf = replicateM

elements :: (Typeable f, Sized f) => [a] -> Shareable f a
elements = aconcat . map pure

growingElements :: Sized f => [a] -> f a
growingElements l = genericIndex l <$> finSized (genericLength l)

inflation
    :: forall a f. Sized f => (Integer -> Integer) -> a -> f (a -> a) -> f a
inflation price zero incr = inflation' 0
  where
    inflation' :: Integer -> f a
    inflation' n
        = pure zero <|> incr <*> splurge (price n) (inflation' (n + 1))

splurge :: (Integral i, Sized f) => i -> f a -> f a
splurge n = foldr (.) id $ genericReplicate n pay
