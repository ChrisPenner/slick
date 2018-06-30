{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson as A
import Data.Foldable
import Development.Shake hiding (Resource)
import GHC.Generics (Generic)
import Helpers

data Post = Post
  { name :: String
  , author :: String
  } deriving (Generic)

instance FromJSON Post

loadPosts :: Action [String]
loadPosts = do
  postNames <- getDirectoryFiles "." ["assets/site/posts//*.md"]
  files <- traverse readFile' postNames
  return files

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    want ["dist/contents.html"]
    postOracle <- simpleJsonCache (const loadPosts)
    -- "site" ~> need ["dist/index.html"]
    "dist/contents.html" %> \out -> do
      liftIO $ print "running contents!"
      posts <- postOracle "posts"
      liftIO . print $ posts
      writeFile' out (fold posts)
    -- "dist/*.html" %> \out -> do
      -- srcIndex <- readFile' ("stuff" </> (dropDirectory1 out))
    --
    --   parts <- getDirectoryFiles "." ["stuff/parts/*.html"]
    --   partFiles <- traverse readFile' parts
    --   let resultIndex = srcIndex <> intercalate "\n---\n" partFiles
    --   writeFile' out resultIndex
    --   
