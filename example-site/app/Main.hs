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
import           Data.List                  as L (intersperse, sortBy, (\\))
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
import           Slick.Build
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
  | Prune       -- ^ run builder from clean slate
  deriving (Eq)

-- | Specific flags for Shake to allow custom keys required for specific logic
--
gFlags :: forall a. [OptDescr (Either a Flags)]
gFlags =
  [ Option "preview" ["preview"] (NoArg $ Right Preview) "running as preview"
  , Option "prune"   ["prune"  ] (NoArg $ Right Prune  ) "run with pruner"
  ]

-- | Specific build rules for the Shake system
--   defines workflow to build the webiste
buildRules :: Foldable t => t Flags -> Rules ()
buildRules flags = do

  let isPreviewMode = Preview `elem` flags

  action $ runAfter $ putStrLn "After Build Actions: "
  when isPreviewMode $ do
    action $ runAfter $ liftIO $ void . forkIO $ do
      stopServer <- newEmptyMVar
      putStrLn $ "Running with Preview"
      serverStart "public" "127.0.0.1" 3030 serverHandler

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


-- | Function to start custom Shake pipeline for execution
--
runSiteBuilder :: ShakeOptions                     -- ^ Options for the Shake builder
               -> [OptDescr (Either String Flags)] -- ^ Converted CLI arguments
               -> Bool                             -- ^ Prune version or No
               -> IO ()
runSiteBuilder shOpts flags isPrune = do
  case isPrune of
    False ->
      -- running regular builder
      shakeArgsWith shOpts flags $
       \flags targets -> do
         return $ Just $ buildRules flags
    True  ->
      -- running builder from clean state
     shakeArgsPruneWith shOpts pruner flags $
       \flags targets -> do
         let rls = Just $ buildRules flags
         -- do additional stuff if needed
         return $ rls

main :: IO ()
main = do
  shakeArgs <- getArgs
  cwd       <- getCurrentDirectory
  let shOpts = shakeOptions {shakeVerbosity = Quiet}

  -- Convert provided flags to Shake compatible by hand
  -- to understand `--prune` option before running shake build
  let (flags, files, errors) = getOpt RequireOrder gFlags shakeArgs
      flags' = map fromEither flags
      isPrune = Prune `elem` flags'

  runSiteBuilder shOpts gFlags isPrune
