{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Language.Javascript.JSaddle.Warp.Extra where

#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Run  ( syncPoint )
import Language.Javascript.JSaddle.Types ( JSM )
import Language.Javascript.JSaddle.WebSockets hiding ( debug )

import Miso.Dev                         ( clearBody )

import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets               as WS

import Paths_frugel

import System.Directory
import System.Environment
import System.FilePath
#endif

-- Split if to make floskell work

#ifndef ghcjs_HOST_OS
debug :: Int -> FilePath -> JSM () -> IO ()
debug port dir f = do
    let staticApp
            = WaiStatic.staticApp $ WaiStatic.defaultFileServerSettings dir
    debugWrapper $ \withRefresh registerContext -> Warp.runSettings
        (Warp.setPort port $ Warp.setTimeout 3600 Warp.defaultSettings)
        =<< jsaddleOr
            defaultConnectionOptions
            (registerContext >> clearBody >> f >> syncPoint)
            (withRefresh $ jsaddleAppWithJsOr (jsaddleJs True) staticApp)
    putStrLn $ "http://localhost:" <> show port

runApp :: JSM () -> IO ()
runApp app = do
    dataDir <- do
        firstExistingDataDir <- foldr
            (\getDir acc -> getDir >>= \dir ->
             ifM (doesPathExist dir) (pure $ Just dir) acc)
            (pure Nothing)
            dataDirs
        maybe ("" <$ print @String "Failed to find web root")
              pure
              firstExistingDataDir
    debug 3708 dataDir app
  where
    dataDirs = [pure "www", getDataDir, do
        exePath <- getExecutablePath
        binDir <- getBinDir
        dataDir <- getDataDir
        pure
            $ takeDirectory (takeDirectory exePath)
            </> makeRelative (takeDirectory binDir) dataDir ]

#else
runApp :: IO () -> IO ()
runApp = id
#endif
