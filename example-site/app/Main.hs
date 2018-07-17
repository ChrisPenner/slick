{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as M
import Data.Monoid
import Data.Set as S
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import Slick

main :: IO ()
main =
  shakeArgs shakeOptions {shakeVerbosity = Chatty} $
    -- Set up caches
   do
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

-- | Represents the template dependencies of the index page
data IndexInfo = IndexInfo
  { posts :: [Post]
  } deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

-- | A JSON serializable representation of a post's metadata
data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , date :: String
  , image :: Maybe String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post

instance ToJSON Post


-- A simple wrapper data-type which implements 'ShakeValue'; 
-- Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath =
  PostFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)

-- | Discover all available post source files
postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert source filepaths into build filepaths
srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . (-<.> ".html")

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData

-- | given a cache of posts this will build a table of contents
buildIndex :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildIndex postCache out = do
  posts <- postNames >>= traverse (postCache . PostFilePath)
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

-- | Find all post source files and tell shake to build the corresponding html
-- pages.
requirePosts :: Action ()
requirePosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

-- Build an html file for a given post given a cache of posts.
buildPost :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  let srcPath = destToSrc out -<.> "md"
      postURL = srcToURL srcPath
  post <- postCache (PostFilePath srcPath)
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON post)
