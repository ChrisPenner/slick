{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Builder( requirePosts
              , loadPost
              , buildIndex
              , buildPost
              ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                (liftM, mzero)
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Aeson.TH                (Options (..), defaultOptions,
                                               deriveJSON)
import           Data.Char
import           Data.Function                (on)
import           Data.List                    (intersperse, sortBy)
import           Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                     as S
import           Data.String.Utils
import qualified Data.Text                    as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Slick
import           Slick.Build
import           Slick.Pandoc
import           System.Console.GetOpt
import           System.Environment
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML     (writeHtml5String)

import           Types

------------------------------------------------------------------------------
-- Builder related functions

-- | Find all post source files and tell shake to build
--   the corresponding html pages.
requirePosts :: Action ()
requirePosts = do
  pPaths <- postPaths
  need ((\p -> srcToDest "dist" p -<.> "html") <$> pPaths)

--------------------------------------------------------------------------------
-- FilePaths loaders

-- | Discover all available post source files
postPaths :: Action [FilePath]
postPaths = getDirectoryFiles "." ["site/posts//*.md"]

--------------------------------------------------------------------------------
-- Page Loaders

-- | Given a post source-file's file path as a cache key, load the Post object
-- for it. This is used with 'jsonCache' to provide post caching.
loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc "site" postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML markdownOptions html5Options . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData

--------------------------------------------------------------------------------
-- Page Builders

-- | given a cache of posts this will build a table of contents
buildIndex :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildIndex postCache out = do
  posts <- postPaths >>= traverse (postCache . PostFilePath)
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

-- Build an html file for a given post given a cache of posts.
buildPost :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  let srcPath = destToSrc "site" out -<.> "md"
      postURL = srcToURL srcPath
  post <- postCache (PostFilePath srcPath)
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON post)

--------------------------------------------------------------------------------
-- Default Pandoc Options

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
