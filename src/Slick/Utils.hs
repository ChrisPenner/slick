module Slick.Utils
  ( getDirectoryPaths
  , fmapFmapOptDescr
  ) where

import           Control.Monad
import           Data.List                  (intersperse, sortBy)
import           Data.List                  as L (intersperse, sortBy, (\\))
import           Data.List                  as L (intersperse, sortBy, (\\))
import           Data.Ord                   (comparing)
import qualified Data.Text                  as T
import           Data.Time
import           Development.Shake
import           Development.Shake.FilePath
import           System.Console.GetOpt
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

fmapFmapOptDescr :: (a -> b) -> OptDescr (Either String a) -> OptDescr (Either String b)
fmapFmapOptDescr f = fmap (fmap f)
