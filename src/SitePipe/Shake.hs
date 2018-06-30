{-# LANGUAGE DeriveFunctor #-}

module SitePipe.Shake where

import Data.Aeson
import qualified Data.Text as T
import Development.Shake hiding (Resource)
import Text.Pandoc

data MetaData = MetaData
  { _url :: String
  , _content :: String
  , _filePath :: String
  } deriving (Show, Eq)

data Resource a =
  Resource String
           a
  deriving (Show, Eq, Functor)

-- resourceLoader' ::
--      (FromJSON v) => (String -> Action String) -> [FilePath] -> Action [v]
-- resourceLoader' fileReader fileNames = do
--   traverse (loadWith fileReader) filenames
-- | loads a file from filepath and applies a given filereader.
loadWith ::
     (FromJSON a)
  => (String -> Action (Resource a)) -- Reader
  -> FilePath
  -> Action (Resource a)
loadWith fileReader filePath = do
  file <- readFile' filePath
  fileReader file

unPandocM :: PandocPure a -> Action a
unPandocM = either (fail . show) return . runPure

markdownReader :: String -> Action (Resource Value)
markdownReader file = do
  pdoc@(Pandoc meta _) <- unPandocM $ readMarkdown def (T.pack file)
  htmlString <- unPandocM $ writeHtml5String def pdoc
  return (Resource (T.unpack htmlString) (toJSON meta))
