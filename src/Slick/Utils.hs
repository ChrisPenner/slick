{-|
Module      : Slick.Utils
Description : Slick helper utilities
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}
module Slick.Utils
  ( getDirectoryPaths
  , convert
  ) where

import Data.Aeson as A
import Development.Shake
import Development.Shake.FilePath

------------------------------------------------------------------------------
-- Helper functions

-- | Given a list of extensions and directories,
--   find all files that match, and return full paths.
getDirectoryPaths :: [FilePath]         -- ^ file pattern like *.md
                  -> [FilePath]         -- ^ directories to look at
                  -> Action [FilePath]
getDirectoryPaths extensions dirs =
  concat <$> mapM getPaths dirs
    where
      getPaths :: FilePath -> Action [FilePath]
      getPaths dir =
        fmap (dir </>) <$>
          getDirectoryFiles dir extensions

-- | Attempt to convert between two JSON serializable objects (or 'Value's).
--   Failure to deserialize fails the Shake build.
convert :: (FromJSON a, ToJSON a, FromJSON b) => a -> Action b
convert a = case fromJSON (toJSON a) of
  A.Success r   -> pure r
  A.Error   err -> fail $ "json conversion error:" ++ err

