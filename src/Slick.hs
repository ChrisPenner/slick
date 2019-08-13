module Slick
  (
  -- * Slick
  -- | This module re-exports everything you need to use Slick

  -- ** Mustache
    compileTemplate'

  -- ** Pandoc
  , PandocReader
  , PandocWriter
  , markdownToHTML
  , markdownToHTML'
  , makePandocReader
  , makePandocReader'
  , loadUsing
  , loadUsing'
  , defaultMarkdownOptions
  , defaultHtml5Options

  -- ** Aeson
  , convert

  -- ** Shake
  , simpleJsonCache
  , simpleJsonCache'
  , jsonCache
  , jsonCache'
  , shakeArgsAlwaysPruneWith
  , pruner

  -- ** Utils
  , getDirectoryPaths

  -- ** Re-exported
  , module Text.Mustache
  )
where

import           Slick.Caching
import           Slick.Mustache
import           Slick.Pandoc
import           Slick.Utils
import           Text.Mustache  hiding ((~>))
