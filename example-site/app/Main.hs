{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text.Lens
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Forward
import           GHC.Generics                 (Generic)
import           Slick
import           System.Directory
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML     (writeHtml5String)
import qualified Data.Map                     as M
import qualified Data.Text                    as T

--------------------------------------------------------------------------------

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
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)


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

writeFileDist :: FilePath -> String -> Action ()
writeFileDist = writeFile' . ("dist" </>)

-- | given a cache of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFileDist "index.html" indexHTML

-- | Find all post source files and tell shake to build
--   the corresponding html pages.
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  postData <- markdownToHTML markdownOptions html5Options . T.pack $ postContent
  let postURL = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withURL = _Object . at "url" ?~ String postURL
  post <- convert . withURL $ postData
  -- Write the post:
  template <- compileTemplate' "site/templates/post.html"
  writeFileDist (url post) . T.unpack $ substitute template (toJSON post)
  return post

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) ("dist" </> filepath)


-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles

main :: IO ()
main = do
  cwd          <- getCurrentDirectory
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty}
  shakeArgsForward shOpts buildRules
