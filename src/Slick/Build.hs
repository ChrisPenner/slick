{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Slick.Build
  ( convert
  , srcToURL
  , destToSrc
  , srcToDest
  , flattenMeta
  , pruner
  , EntityFilePath(..)
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List                  as L (intersperse, sortBy, (\\))
import qualified Data.Text                  as T
import           Development.Shake
import           Development.Shake          hiding (Resource)
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               hiding (Meta)
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           Text.Pandoc
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Shared

import           Slick.Utils

--------------------------------------------------------------------------------

-- | Generalized version of filepath
newtype EntityFilePath a =
  EntityFilePath String
   deriving (Show, Eq,  Generic, Hashable, Binary, NFData)

-- | convert 'build' filepaths into source file filepaths
destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

-- | convert source filepaths into build filepaths
srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

-- | convert a source file path into a URL
srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . (-<.> ".html")

--------------------------------------------------------------------------------

-- | Attempt to convert between two JSON serializable objects (or 'Value's).
--   Failure to deserialize fails the Shake build.
convert :: (FromJSON a, ToJSON a, FromJSON b) => a -> Action b
convert a = case fromJSON (toJSON a) of
  Success r   -> pure r
  Error   err -> fail $ "json conversion error:" ++ err

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
--   text objects into plain strings along the way.
flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
 where
  go :: MetaValue -> Value
  go (MetaMap     m) = toJSON $ fmap go m
  go (MetaList    m) = toJSONList $ fmap go m
  go (MetaBool    m) = toJSON m
  go (MetaString  m) = toJSON m
  go (MetaInlines m) = toJSON $ stringify m
  go (MetaBlocks  m) = toJSON $ stringify m

--------------------------------------------------------------------------------

-- | Custom cleanup for the Shake builder
--
pruner :: [FilePath] -> IO ()
pruner listOfFiles = do
  present <- listFilesRecursive "public"
  mapM_ removeFile $ (toStandard <$> present) L.\\ (toStandard <$> listOfFiles)
  putStrLn "PRUNER  "
