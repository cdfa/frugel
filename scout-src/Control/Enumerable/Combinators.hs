module Control.Enumerable.Combinators where

import Control.Enumerable

import qualified Relude.Unsafe as Unsafe

vectorOf :: Applicative f => Int -> Shareable f a -> Shareable f [a]
vectorOf = replicateM

elements :: (Typeable f, Sized f) => [a] -> Shareable f a
elements xs = (xs Unsafe.!!) . fromInteger <$> finSized (toInteger $ length xs)

inflation :: Sized f => (Int -> Int) -> a -> f (a -> a) -> f a
inflation price zero incr = inflation' 0
  where
    inflation' n
        = pure zero <|> incr <*> splurge (price n) (inflation' (n + 1))

splurge :: Sized f => Int -> f a -> f a
splurge n = foldr (.) id $ replicate n pay
