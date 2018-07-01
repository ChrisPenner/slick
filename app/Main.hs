{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
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
  , tags :: [String]
  } deriving (Generic)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  } deriving (Generic)

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
  convert withURL

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
      let indexInfo = IndexInfo {posts = ps, tags = []}
          indexF = T.unpack $ substitute indexT (toJSON indexInfo)
      writeFile' out indexF
    "posts" ~> do
      pNames <- postNames
      need ((\p -> srcToDest p -<.> "html") <$> pNames)
    "dist/posts//*.html" %> \out -> do
      post <- postCache out
      template <- compileTemplate' "site/templates/post.html"
      writeFile' out . T.unpack $ substitute template (toJSON post)
