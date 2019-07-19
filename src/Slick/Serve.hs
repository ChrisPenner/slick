{-# LANGUAGE OverloadedStrings #-}

module Slick.Serve ( runServerStatic
                   , runServerWS
                   , wsAppHandler
                   ) where

import           Control.Concurrent.MVar
import           Control.Monad                  (forM_, forever, liftM, mzero,
                                                 void, when, (>=>))
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import           Data.String
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Network.HTTP.Types.Status      (Status)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

--------------------------------------------------------------------------------

type WSAppState = MVar [WS.Connection]

-- | Basic server configuration
--
runServer :: String               -- ^ Host to bind on
          -> Int                  -- ^ Port to listen on
          -> IO ()                -- ^ Blocks forever
runServer host port = do
  Warp.runSettings
    warpSettings $
    Static.staticApp (Static.defaultFileServerSettings "")
  where
    warpSettings =
      Warp.setLogger noLog
        $ Warp.setHost (fromString host)
        $ Warp.setPort port Warp.defaultSettings

-- | Run simple Http app for
--   preview local files for generated site
runServerStatic :: FilePath             -- ^ Directory to serve
                -> String               -- ^ Host to bind on
                -> Int                  -- ^ Port to listen on
                -> IO ()                -- ^ Blocks forever
runServerStatic directory host port = do
  Warp.runSettings
    warpSettings $
      Static.staticApp (Static.defaultFileServerSettings directory)
  where
    warpSettings =
      Warp.setLogger noLog
        $ Warp.setHost (fromString host)
        $ Warp.setPort port Warp.defaultSettings

-- | Run server with Websocket connection
--   for live reloading
runServerWS :: FilePath                 -- ^ Directory to serve
            -> String                   -- ^ Host to bind on
            -> Int                      -- ^ Port to listen on
            -> (WSAppState -> WS.Connection -> IO ()) -- ^ Websockets handler
            -> IO ()
runServerWS directory host portWS wsApp = do
  wsState <- newMVar []
  Warp.runSettings
    warpSettings $
      Wai.WS.websocketsOr
        WS.defaultConnectionOptions          -- Compression disabled
        (WS.acceptRequest >=>                -- App for websocket requests
          \conn -> do
            WS.forkPingThread conn 30
              >> wsApp wsState conn)
        (httpApp directory)                  -- App for non-websocket requests
  where
    warpSettings =
      Warp.setLogger noLog
        $ Warp.setHost (fromString host)
        $ Warp.setPort portWS Warp.defaultSettings

-- | Http application
--
httpApp :: FilePath -> Wai.Application
httpApp directory =
  Static.staticApp $
     (Static.defaultFileServerSettings directory)

-- | Handler for websocket server to brodcast update query
--   for all connected browser pages
wsAppHandler :: WSAppState -> WS.Connection -> IO ()
wsAppHandler wsState conn = do
  forever $ do
    modifyMVar_ wsState (\l -> return (conn:l))
    l <- readMVar wsState

    liftIO $ putStrLn $ "preview-server: putting conn to list of connections"
    liftIO $ putStrLn $ "length l: " ++ show (length l)

    msg <- WS.receiveData conn
    liftIO $ putStrLn $ "preview-server: RCV " ++ show (msg :: T.Text)

    case msg == "update" of
      False -> return $ ()
      True  -> do
        broadcast wsState ("preview-server: " <> msg)
        -- drop all existing connection, as after browser reload
        -- wsconnection is not valid, instead a new one established
        modifyMVar_ wsState (\l -> return [])

-- | Send message to all connected WS clients
broadcast :: WSAppState -> T.Text -> IO ()
broadcast s msg = do
  l <- readMVar s
  forM_ l $ \conn -> do
    WS.sendTextData conn msg

-- |
noLog :: Wai.Request -> Status -> Maybe Integer -> IO ()
noLog _ _ _ = return ()
