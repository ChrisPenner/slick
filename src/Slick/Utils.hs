{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Slick.Utils
  ( getDirectoryPaths
  ) where

import           Control.Monad
import           Data.List                  (intersperse, sortBy)
import           Data.List                  as L (intersperse, sortBy, (\\))
import           Data.Ord                   (comparing)
import qualified Data.Text                  as T
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory
import           System.Directory.Extra     (listFilesRecursive, removeFile)
import           System.Environment

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
