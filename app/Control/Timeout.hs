{-# LANGUAGE CPP #-}

module Control.Timeout where

import Control.Concurrent
import Control.Exception

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import GHC.Event
#endif

unlessFinishedIn :: Int -> IO () -> (MVar ThreadId -> IO a) -> IO a
unlessFinishedIn delay handler action
    | delay <= 0 = do
        handlerThreadVar <- newMVar =<< forkIO handler
        action handlerThreadVar
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
unlessFinishedIn delay handler action = do
    timerManager <- getSystemTimerManager
    handlerThreadVar <- newEmptyMVar
    let handleTimeout = do
            timedActionNotFinished <- isEmptyMVar handlerThreadVar
            when timedActionNotFinished . void . forkIO $ do
                timedActionNotFinished2
                    <- tryPutMVar handlerThreadVar =<< myThreadId
                when timedActionNotFinished2 handler
        cleanupTimeout key = uninterruptibleMask_ $ do
            handlerNotStarted <- tryPutMVar handlerThreadVar
                $ error "handler thread id evaluated by handler"
            when handlerNotStarted $ do
                unregisterTimeout timerManager key
    bracket (registerTimeout timerManager delay handleTimeout) cleanupTimeout
        . const
        $ action handlerThreadVar
#else
unlessFinishedIn delay handler action = do
    handlerThreadVar <- newEmptyMVar
    let handleTimeout unmask = do
            void . unmask $ threadDelay delay
            putMVar handlerThreadVar =<< myThreadId
            handler
        cleanupHandler handlerId = uninterruptibleMask_ $ do
            handlerNotStarted <- isEmptyMVar handlerThreadVar
            when handlerNotStarted $ killThread handlerId
    bracket (forkIOWithUnmask handleTimeout) cleanupHandler . const
        $ action handlerThreadVar
#endif
