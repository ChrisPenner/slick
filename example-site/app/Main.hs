{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import           Development.Shake.Forward
import           Development.Shake.Util     (shakeArgsPruneWith)
import           GHC.Generics               (Generic)
import           Slick
import           Slick.Serve
import           System.Console.GetOpt
import           System.Directory
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           System.Environment
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML     (writeHtml5String)

import           Builder

--------------------------------------------------------------------------------


-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
markdownOptions =
  def { readerExtensions = exts }
   where
     exts = mconcat
       [ extensionsFromList
         [ Ext_yaml_metadata_block
         , Ext_fenced_code_attributes
         , Ext_auto_identifiers
         ]
       , githubMarkdownExtensions
       ]

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options =
  def { writerHighlightStyle = Just tango
      , writerExtensions     = writerExtensions def
      }

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
loadPost :: FilePath -> Action Post
loadPost srcPath = do
  postData <- readFile' srcPath >>= markdownToHTML markdownOptions html5Options . T.pack
  let postURL = T.pack . srcToURL $ srcPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData

srcToURL = id

buildStatic :: Action ()
buildStatic = return ()

buildPosts :: Action ()
buildPosts = return ()


-- | Represents the template dependencies of the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | A JSON serializable representation of a post's metadata
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , srcPath :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- Build an html file for a given post given a cache of posts.
writePost :: Post -> Action ()
writePost post = cacheAction post $ do
  liftIO . putStrLn $ "writing " <> srcPath post <> "!"
  template <- compileTemplate' "site/templates/post.html"
  writeFile' ("dist" </> dropDirectory1 (srcPath post) -<.> "html") . T.unpack $ substitute template (toJSON post)

-- | given a cache of posts this will build a table of contents
buildIndex :: [Post] -> FilePath -> Action ()
buildIndex posts' out = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

-- | Find all post source files and tell shake to build
--   the corresponding html pages.
loadPosts :: Action [Post]
loadPosts = cacheAction ("load-posts" :: T.Text) $ do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths loadPost

-- | Specific build rules for the Shake system
--   defines workflow to build the webiste
buildRules :: Action ()
buildRules = do
  -- Site specific Shake rules that
  -- define steps to build output website

  allPosts <- loadPosts
  -- postCache <- jsonCache' loadPost

  void $ forP allPosts writePost

  -- parallel [buildStatic, void $ forP allPosts writePost, buildIndex]

  -- Require all the things we need to build the whole site
  -- "site" ~> need ["static", "posts", "dist/index.html"]

  -- Require all static assets
  -- "static" ~> do
  --   staticFiles <-
  --     getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
  --   need (("dist" </>) . dropDirectory1 <$> staticFiles)

  -- -- Rule for handling static assets, just copy them from source to dest
  -- ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
  --   copyFileChanged ("site" </> dropDirectory1 out) out

  --   -- Find and require every post to be built
  -- "posts" ~> requirePosts

  -- -- build the main table of contents
  -- "dist/index.html" %> buildIndex postCache

  -- -- rule for actually building posts
  -- "dist/posts//*.html" %> buildPost postCache

  -- -- required to build `dist`
  -- want ["site"]

main :: IO ()
main = do
  cwd          <- getCurrentDirectory

  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty}

  shakeArgsForward shOpts buildRules
