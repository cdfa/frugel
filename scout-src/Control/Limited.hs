{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- Provides a Monad and newtype for limiting recursion depth. Example:
-- take :: Int -> [a] -> [a]
-- take n xs = runIdentity $ runLimiterT (fromMaybe [] <$> draw (take' xs)) n
--   where
--     take' [] = pure $ Limited []
--     take' (y : ys) = Limited . (y :) . fromMaybe [] <$> draw (take' ys)
module Control.Limited
    ( Limit(..)
    , Limiter
    , LimiterT
    , Limited(..)
    , MonadLimiter(..)
    , runLimiter
    , runLimiterT
    , usingLimiter
    , usingLimiterT
    , mapLimiterT
    ) where

import Control.Monad.Writer

import Prelude        hiding ( pass )

data Limit = Infinity | Only Int
    deriving ( Show, Eq )

newtype LimiterT m a = LimiterT { unLimiterT :: ReaderT Limit m a }
    deriving ( Functor, Applicative, Monad, MonadTrans )

type Limiter = LimiterT Identity

newtype Limited a = Limited { unLimited :: a }

instance Ord Limit where
    _ <= Infinity = True
    Infinity <= _ = False
    Only x <= Only y = x <= y

class Monad m => MonadLimiter m where
    askLimit :: m Limit
    draw :: m (Limited a) -> m (Maybe a)

instance Monad m => MonadLimiter (LimiterT m) where
    askLimit = LimiterT ask
    draw limited = do
        budget <- askLimit
        if budget <= Only 0
            then pure Nothing
            else (Just . unLimited) <.> LimiterT . local predLimit
                $ unLimiterT limited

-- instance MonadReader r m => MonadReader r (LimiterT m)
instance MonadWriter w m => MonadWriter w (LimiterT m) where
    writer = LimiterT . writer
    listen = mapLimiterT listen
    pass = mapLimiterT pass
    tell = LimiterT . tell

instance MonadLimiter m => MonadLimiter (ReaderT r m) where
    askLimit = lift askLimit
    draw = mapReaderT draw

predLimit :: Limit -> Limit
predLimit Infinity = Infinity
predLimit (Only x) = Only $ pred x

runLimiter :: LimiterT Identity a -> Limit -> a
runLimiter = runReader . unLimiterT

usingLimiter :: Limit -> LimiterT Identity a -> a
usingLimiter = flip runLimiter

runLimiterT :: LimiterT m a -> Limit -> m a
runLimiterT = runReaderT . unLimiterT

usingLimiterT :: Limit -> LimiterT m a -> m a
usingLimiterT = flip runLimiterT

mapLimiterT :: (m a -> n b) -> LimiterT m a -> LimiterT n b
mapLimiterT f = LimiterT . mapReaderT f . unLimiterT
