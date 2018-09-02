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
import Development.Shake hiding (Resource)
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
    allPostsCache <-
      simpleJsonCache'
        (SortedPostsCache ())
        (sortByDate <$> (postNames >>= traverse (postCache . PostFilePath)))
    sortedPostURLsCache <-
      simpleJsonCache'
        (SortedPostURLsCache ())
        (fmap (url :: Post -> String) <$> allPostsCache)
    allTagsCache <- simpleJsonCache' (TagCache ()) (getTags <$> allPostsCache)
    -- Require all the things we need to build the whole site
    "site" ~> need ["static", "posts", "tags", "dist/index.html"]
    -- Require all static assets
    "static" ~> do
      staticFiles <-
        getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
      need (("dist" </>) . dropDirectory1 <$> staticFiles)
    -- Rule for handling static assets, just copy them from source to dest
    ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
    -- build the main table of contents
    "dist/index.html" %> buildIndex allPostsCache allTagsCache
     -- Find and require every post to be built
    "posts" ~> findPosts
    -- Find and require every tag to be built
    "tags" ~> findTags allTagsCache
     -- rule for actually building tags
    "dist/tag//*.html" %> buildTag allTagsCache
     -- rule for actually building posts
    "dist/posts//*.html" %> buildPost postCache sortedPostURLsCache

data IndexInfo = IndexInfo
  { posts :: [Post]
  , tags :: [Tag]
  } deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

data Tag = Tag
  { tag :: String
  , posts :: [Post]
  , url :: String
  } deriving (Generic, Show)

instance FromJSON Tag

instance ToJSON Tag

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , tags :: [String]
  , nextPostURL :: Maybe String
  , prevPostURL :: Maybe String
  , isoDate :: String
  , date :: String
  , srcPath :: String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post where
  parseJSON v = do
    let title = v ^. key "title" . _String . unpacked
        author = v ^. key "author" . _String . unpacked
        date = v ^. key "date" . _String . unpacked
        isoDate = formatDate date
        content = v ^. key "content" . _String . unpacked
        url = v ^. key "url" . _String . unpacked
        tags = v ^.. key "tags" . values . _String . unpacked
        nextPostURL = Nothing
        prevPostURL = Nothing
        srcPath = v ^. key "srcPath" . _String . unpacked
     in return Post {..}

instance ToJSON Post

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension

loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= markdownToHTML . T.pack
  let postURL = T.pack . srcToURL $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData

buildIndex :: Action [Post] -> Action [Tag] -> FilePath -> Action ()
buildIndex allPosts allTags out = do
  indexT <- compileTemplate' "site/templates/index.html"
  posts <- allPosts
  tags <- allTags
  let indexInfo = IndexInfo {posts, tags}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

findPosts :: Action ()
findPosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

findTags :: Action [Tag] -> Action ()
findTags allTags = do
  ts <- allTags
  let toTarget Tag {tag} = "dist/tag/" ++ tag ++ ".html"
  need (toTarget <$> ts)

buildTag :: Action [Tag] -> FilePath -> Action ()
buildTag allTags out = do
  tagList <- allTags
  let tagName = (dropExtension . dropDirectory1 . dropDirectory1) out
      findTag t@Tag {tag}
        | tag == tagName = First (Just t)
      findTag _ = First Nothing
      maybeTag = getFirst $ foldMap findTag tagList
  t <-
    case maybeTag of
      Nothing -> fail $ "could not find tag: " <> tagName
      Just t' -> return t'
  tagTempl <- compileTemplate' "site/templates/tag.html"
  writeFile' out . T.unpack $ substitute tagTempl (toJSON t)

buildPost ::
     (PostFilePath -> Action Post) -> Action [String] -> FilePath -> Action ()
buildPost postCache sortedPostURLsCache out = do
  let srcPath = destToSrc out -<.> "md"
      postURL = srcToURL srcPath
  (prevPostURL, nextPostURL) <- getNeighbours postURL <$> sortedPostURLsCache
  post <- postCache (PostFilePath srcPath)
  let withNeighbours = post {nextPostURL, prevPostURL}
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON withNeighbours)

getNeighbours :: String -> [String] -> (Maybe String, Maybe String)
getNeighbours i xs =
  let ms = pure <$> xs
   in go ([Nothing] <> ms <> [Nothing])
  where
    go (before:Just current:after:_)
      | current == i = (before, after)
    go (_:rest) = go rest
    go [] = (Nothing, Nothing)

getTags :: [Post] -> [Tag]
getTags posts =
  let tagToPostsSet = M.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap S.toList tagToPostsSet
      tagObjects =
        foldMapWithKey
          (\tag ps -> [Tag {tag, posts = ps, url = "/tag/" <> tag}])
          tagToPostsList
   in tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {tags} = M.unionsWith mappend (embed p <$> tags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = M.singleton tag (S.singleton post)

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` isoDate

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

newtype SortedPostsCache =
  SortedPostsCache ()
  deriving (Show, Eq, Hashable, Binary, NFData)

newtype SortedPostURLsCache =
  SortedPostURLsCache ()
  deriving (Show, Eq, Hashable, Binary, NFData)

newtype TagCache =
  TagCache ()
  deriving (Show, Eq, Hashable, Binary, NFData)

newtype PostFilePath =
  PostFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)
