{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Control.Lens
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Map                   as M
import           Data.Monoid
import           Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Util     (shakeArgsPruneWith)
import           GHC.Generics               (Generic)
import           Slick
import           System.Console.GetOpt
import           System.Directory
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           System.Environment

import           Builder
import           Types

--------------------------------------------------------------------------------

-- | List our custom keys that we need to manage additional behaviour
--
data Flags =
    Preview     -- ^ key to run Warp server
  | Watch       -- ^ watch for changes
  deriving (Eq)

-- | Specific flags for Shake to allow custom keys required for specific logic
--
flags :: forall a. [OptDescr (Either a Flags)]
flags =
  [ Option "" ["preview"      ] (NoArg $ Right Preview    ) "running as preview"
  , Option "" ["watch"        ] (NoArg $ Right Watch      ) "running with watch"
  ]

buildRules :: Foldable t => t Flags -> Rules ()
buildRules flags = do
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
               -> IO ()
runSiteBuilder shOpts flags =
  shakeArgsWith shOpts flags $
   \flags targets -> do
     let rls = Just $ buildRules flags
     return $ rls

main :: IO ()
main = do
  shakeArgs <- getArgs
  cwd       <- getCurrentDirectory
  let shOpts  = shakeOptions {shakeVerbosity = Quiet}

  runSiteBuilder shOpts flags
