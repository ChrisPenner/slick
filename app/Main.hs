{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Map as M
import Data.Monoid
import Data.Set as S
import qualified Data.Text as T
import Data.Text.Lens
import Development.Shake hiding (Resource)
import Development.Shake.FilePath
import GHC.Generics (Generic)

import Helpers
import SitePipe.Shake
import Text.Mustache hiding ((~>))
import Text.Mustache.Shake

main :: IO ()
main =
  shakeArgs shakeOptions $
    -- Set up caches
   do
    postCache <- simpleJsonCache loadPost
    let allPosts = postNames >>= traverse postCache
        allTags = getTags <$> allPosts
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
    "dist/index.html" %> buildIndex allPosts allTags
     -- Find and require every post to be built
    "posts" ~> findPosts
    -- Find and require every tag to be built
    "tags" ~> findTags allTags
     -- rule for actually building tags
    "dist/tag//*.html" %> buildTag allTags
     -- rule for actually building posts
    "dist/posts//*.html" %> buildPost postCache

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
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post where
  parseJSON v =
    let title = v ^. key "title" . _String . unpacked
        author = v ^. key "author" . _String . unpacked
        content = v ^. key "content" . _String . unpacked
        url = v ^. key "url" . _String . unpacked
        tags = v ^.. key "tags" . values . _String . unpacked
     in return Post {..}

instance ToJSON Post

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

loadPost :: FilePath -> Action Post
loadPost postPath = do
  postData <- readFile' (destToSrc postPath -<.> "md") >>= markdownReader
  let postURL = T.pack . ("/" ++) . dropDirectory1 . dropExtension $ postPath
      withURL = postData & _Object . at "url" ?~ String postURL
  convert withURL

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

buildPost :: (String -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  post <- postCache out
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON post)

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
