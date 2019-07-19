{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Slick.Utils
  ( renderToDay
  , fromEither
  , getDirectoryPaths
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

-- | Generalized sorting function
sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs =
  liftM (Prelude.map fst . sortBy (comparing snd)) $ mapM (\x -> liftM (x,) (f x)) xs

-- | Render timestamp to the format required by page design
renderToDay :: FormatTime t => t -> String
renderToDay = formatTime defaultTimeLocale "%B %e, %Y"

-- | Take an Either,  and return the value inside it
fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

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
