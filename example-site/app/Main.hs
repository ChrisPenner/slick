{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad              (forM_, forever, liftM, mzero, void,
                                             when)
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Either.Utils
import           Data.Function              (on)
import           Data.List                  as L (intercalate, intersperse,
                                                  sortBy, (\\))
import qualified Data.Map                   as M
import           Data.Monoid
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Util     (shakeArgsPruneWith)
import           GHC.Generics               (Generic)
import           Slick
import           Slick.Serve
import           System.Console.GetOpt
import           System.Directory
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           System.Environment

import           Builder

--------------------------------------------------------------------------------

-- | List our custom keys that we need to manage additional behaviour
--
data Flags =
    Preview     -- ^ key to run warp server
--  | OtherFlag -- ^ run builder from clean slate
  deriving (Eq)

-- | Specific flags for Shake to allow custom keys required for specific logic
--
gFlags :: forall a. [OptDescr (Either a Flags)]
gFlags =
  [ Option "P" ["preview"] (NoArg $ Right Preview) "running as preview"
--, Option "O" ["OtherFlag"] (NoArg $ Right OtherFlag  ) "run with some other flag"
  ]

-- | Specific build rules for the Shake system
--   defines workflow to build the webiste
buildRules :: Foldable t => t Flags -> Rules ()
buildRules flags = do
  let isPreviewMode = Preview `elem` flags
  action $ runAfter $ putStrLn "After Build Actions: "
  when isPreviewMode $ do
    action $ runAfter $ liftIO $ do
      th <- forkIO $ do
        stopServer <- newEmptyMVar
        let previewPort = 3030
        let previewHost = "localhost"
        putStrLn $ "Running with Preview on http://" <> previewHost <> ":" <> show previewPort
        serverStart "dist" previewHost previewPort serverHandler

      forever $
        threadDelay 100000

  -- Site specific Shake rules that
  -- define steps to build output website

  postCache <- jsonCache' loadPost

  -- Require all the things we need to build the whole site
  "site" ~> need ["static", "posts", "dist/index.html"]

  -- Require all static assets
  "static" ~> do
    staticFiles <-
      getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
    need (("dist" </>) . dropDirectory1 <$> staticFiles)

  -- Rule for handling static assets, just copy them from source to dest
  ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
    copyFileChanged ("site" </> dropDirectory1 out) out

    -- Find and require every post to be built
  "posts" ~> requirePosts

  -- build the main table of contents
  "dist/index.html" %> buildIndex postCache

  -- rule for actually building posts
  "dist/posts//*.html" %> buildPost postCache

  -- required to build `dist`
  want ["site"]

-- | Function to start custom Shake pipeline for execution
runSiteBuilder :: ShakeOptions                     -- ^ Options for the Shake builder
               -> [OptDescr (Either String Flags)] -- ^ Converted CLI arguments
               -> IO ()
runSiteBuilder shOpts flags =
  -- running builder from clean state
  shakeArgsAlwaysPruneWith shOpts (pruner "dist") flags $
    \flags targets -> do
      let rls = Just $ buildRules flags
      -- do additional stuff if needed
      return $ rls

main :: IO ()
main = do
  shakeArgsRaw <- getArgs
  cwd          <- getCurrentDirectory

  let shOpts = shakeOptions { shakeVerbosity = Chatty}

  runSiteBuilder shOpts gFlags
