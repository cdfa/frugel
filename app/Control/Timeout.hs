{-# LANGUAGE CPP #-}

module Control.Timeout where

import Control.Concurrent
import Control.Exception

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import GHC.Event
#endif

unlessFinishedIn :: Int -> IO () -> IO a -> IO a
unlessFinishedIn delay handler action | delay <= 0 = do
    void $ forkIO handler
    action
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
unlessFinishedIn delay handler action = do
    timerManager <- getSystemTimerManager
    lock <- newEmptyMVar
    let handleTimeout = do
            timedActionNotFinished <- isEmptyMVar lock
            when timedActionNotFinished . void . forkIO $ do
                timedActionNotFinished2 <- tryPutMVar lock ()
                when timedActionNotFinished2 handler
        cleanupTimeout key = uninterruptibleMask_ $ do
            handlerNotStarted <- tryPutMVar lock ()
            when handlerNotStarted $ do
                unregisterTimeout timerManager key
    bracket (registerTimeout timerManager delay handleTimeout) cleanupTimeout
        $ const action
#else
unlessFinishedIn delay handler action = do
    handlerThreadVar <- newEmptyMVar
    let handleTimeout unmask = do
            void . unmask $ threadDelay delay
            putMVar handlerThreadVar ()
            handler
        cleanupHandler handlerId = uninterruptibleMask_ $ do
            handlerNotStarted <- isEmptyMVar handlerThreadVar
            when handlerNotStarted $ killThread handlerId
    bracket (forkIOWithUnmask handleTimeout) cleanupHandler $ const action
#endif
