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
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)

import Helpers
import SitePipe.Shake
import Text.Mustache hiding ((~>))
import Text.Mustache.Shake

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
  postMeta <- readFile' (destToSrc postPath -<.> "md") >>= markdownReader
  let postURL = T.pack . ("/" ++) . dropDirectory1 . dropExtension $ postPath
      withURL = postMeta & _Object . at "url" ?~ String postURL
  convert withURL

newtype TagQuery =
  TagQuery ()
  deriving (Show, Eq, Ord, Hashable, Binary, NFData)

loadTags :: (FilePath -> Action Post) -> TagQuery -> Action [Tag]
loadTags postCache (TagQuery _) = do
  pNames <- postNames
  ps <- traverse postCache pNames
  return $ getTags ps

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    postCache <- simpleJsonCache loadPost
    tagsCache <- ($ TagQuery ()) <$> jsonOracle (loadTags postCache)
    "static" ~> do
      staticFiles <-
        getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
      need (("dist" </>) . dropDirectory1 <$> staticFiles)
    ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
    "site" ~> need ["static", "posts", "tags", "dist/index.html"]
    "dist/index.html" %> \out -> do
      indexT <- compileTemplate' "site/templates/index.html"
      pNames <- postNames
      ps <- forP pNames postCache
      tags <- tagsCache
      let indexInfo = IndexInfo {posts = ps, tags}
          indexF = T.unpack $ substitute indexT (toJSON indexInfo)
      writeFile' out indexF
    "posts" ~> do
      pNames <- postNames
      need ((\p -> srcToDest p -<.> "html") <$> pNames)
    "tags" ~> do
      ts <- tagsCache
      let toTarget Tag {tag} = "dist/tag/" ++ tag ++ ".html"
      need (toTarget <$> ts)
    "dist/tag//*.html" %> \out -> do
      tagList <- tagsCache
      let tagName = (dropExtension . dropDirectory1 . dropDirectory1) out
          findTag t@Tag {tag}
            | tag == tagName = First (Just t)
          findTag _ = First Nothing
          tMaybe = getFirst $ foldMap findTag tagList
      t <-
        case tMaybe of
          Nothing -> fail $ "could not find tag: " <> tagName
          Just t' -> return t'
      tagTempl <- compileTemplate' "site/templates/tag.html"
      liftIO $ print t
      writeFile' out . T.unpack $ substitute tagTempl (toJSON t)
    "dist/posts//*.html" %> \out -> do
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
