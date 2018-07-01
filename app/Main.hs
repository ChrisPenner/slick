{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson as A
import Development.Shake hiding (Resource)
import Development.Shake.FilePath
import GHC.Generics (Generic)

-- import Helpers
import SitePipe.Shake

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
    "site" ~>
  -- postOracle <- simpleJsonCache (const loadPosts)
     do
      pNames <- postNames
      liftIO $ print pNames
      need ((\p -> "dist" </> dropDirectory1 p -<.> "html") <$> pNames)
    "dist/posts//*.html" %> \out -> do
      liftIO $ print out
      contents <- readFile' ("site" </> dropDirectory1 out -<.> "md")
      Post {content = f} <- markdownReader contents
      writeFile' out f
