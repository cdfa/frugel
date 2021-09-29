{-# LANGUAGE CPP #-}

module Language.Javascript.JSaddle.Warp.Extra where

#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Run  ( syncPoint )
import Language.Javascript.JSaddle.Types ( JSM )
import Language.Javascript.JSaddle.WebSockets hiding ( debug )

import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets               as WS

debug :: Int -> FilePath -> JSM () -> IO ()
debug port dir f = do
    let staticApp
            = WaiStatic.staticApp $ WaiStatic.defaultFileServerSettings dir
    debugWrapper $ \withRefresh registerContext -> Warp.runSettings
        (Warp.setPort port $ Warp.setTimeout 3600 Warp.defaultSettings)
        =<< jsaddleOr
            defaultConnectionOptions
            (registerContext >> f >> syncPoint)
            (withRefresh $ jsaddleAppWithJsOr (jsaddleJs True) staticApp)
    putStrLn $ "http://localhost:" <> show port

runApp :: JSM () -> IO ()
runApp = debug 3708 "www"

#else
runApp :: IO () -> IO ()
runApp = id
#endif

