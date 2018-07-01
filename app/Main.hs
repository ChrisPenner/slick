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
import Data.Aeson.Types as A
import Data.Map as M
import Data.Set as S
import qualified Data.Text as T
import Development.Shake hiding (Resource)
import Development.Shake.FilePath
import GHC.Generics (Generic)

import Helpers
import SitePipe.Shake
import Text.Mustache hiding ((~>))
import Text.Mustache.Shake

data IndexInfo = IndexInfo
  { posts :: [Post]
  , tags :: [Value]
  } deriving (Generic)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , tags :: [String]
  } deriving (Generic, Eq, Ord)

instance FromJSON Post

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
      withTags = withURL & _Object . at "tags" ?~ A.emptyArray
  convert withTags

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    postCache <- simpleJsonCache loadPost
    "static" ~> do
      staticFiles <-
        getDirectoryFiles "." ["site/css//*", "site/js//*", "site/images//*"]
      need (("dist" </>) . dropDirectory1 <$> staticFiles)
    ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
    "site" ~> need ["static", "posts", "dist/index.html"]
    "dist/index.html" %> \out -> do
      indexT <- compileTemplate' "site/templates/index.html"
      pNames <- postNames
      ps <- forP pNames postCache
      let indexInfo = IndexInfo {posts = ps, tags = getTags ps}
          indexF = T.unpack $ substitute indexT (toJSON indexInfo)
      writeFile' out indexF
    "posts" ~> do
      pNames <- postNames
      need ((\p -> srcToDest p -<.> "html") <$> pNames)
    "dist/posts//*.html" %> \out -> do
      post <- postCache out
      template <- compileTemplate' "site/templates/post.html"
      writeFile' out . T.unpack $ substitute template (toJSON post)

getTags :: [Post] -> [Value]
getTags posts =
  let tagToPostsSet = M.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap S.toList tagToPostsSet
      tagObjects =
        foldMapWithKey
          (\tag ps ->
             [A.object [("tag", String (T.pack tag)), ("posts", toJSONList ps)]])
          tagToPostsList
   in tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {tags} = M.unionsWith mappend (embed p <$> tags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = M.singleton tag (S.singleton post)
