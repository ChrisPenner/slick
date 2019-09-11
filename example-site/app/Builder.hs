{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Builder() where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                (liftM, mzero)
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Aeson.TH                (Options (..), defaultOptions,
                                               deriveJSON)
import           Data.Char
import           Data.Function                (on)
import           Data.List                    (intersperse, sortBy)
import           Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                     as S
import           Data.String.Utils
import qualified Data.Text                    as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Slick
import           Slick.Pandoc
import           System.Console.GetOpt
import           System.Environment
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML     (writeHtml5String)

import           Types

------------------------------------------------------------------------------
-- Builder related functions


--------------------------------------------------------------------------------
-- FilePaths loaders

-- | Discover all available post source files
postPaths :: Action [FilePath]
postPaths = getDirectoryFiles "." ["site/posts//*.md"]

--------------------------------------------------------------------------------
-- Page Loaders

--------------------------------------------------------------------------------
-- Page Builders

--------------------------------------------------------------------------------
-- Default Pandoc Options

