{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson as A
import qualified Data.Text as T
import Development.Shake hiding (Resource)
import Development.Shake.FilePath
import GHC.Generics (Generic)

-- import Helpers
import SitePipe.Shake
import Text.Mustache hiding ((~>))
import Text.Mustache.Shake

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  } deriving (Generic)

instance FromJSON Post

instance ToJSON Post

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

loadPosts :: Action [Post]
loadPosts = do
  pNames <- postNames
  files <- traverse readFile' pNames
  traverse markdownReader files

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    want ["dist/contents.html"]
    "static" ~> do
      staticFiles <-
        getDirectoryFiles "." ["site//*.css", "site//*.js", "site//*.png"]
      need (("dist" </>) . dropDirectory1 <$> staticFiles)
    ["dist//*.css", "dist//*.png", "dist//*.js"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
    "site" ~>
  -- postOracle <- simpleJsonCache (const loadPosts)
     do need ["static", "posts"]
    "posts" ~> do
      pNames <- postNames
      need ((\p -> "dist" </> dropDirectory1 p -<.> "html") <$> pNames)
    "dist/posts//*.html" %> \out -> do
      liftIO $ print out
      contents <- readFile' ("site" </> dropDirectory1 out -<.> "md")
      post <- markdownReader contents :: Action Post
      template <- compileTemplate' "site/templates/post.html"
      writeFile' out . T.unpack $ substitute template (toJSON post)
