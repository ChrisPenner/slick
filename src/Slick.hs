-- | Slick
--  This module re-exports everything you need to use Slick

module Slick
  ( PandocReader
  , PandocWriter
  , markdownToHTML
  , markdownToHTML'
  , makePandocReader
  , makePandocReader'
  , loadUsing
  , loadUsing'
  , markdownOptions
  , html5Options

  -- ** Aeson
  , convert

  -- ** Shake
  , simpleJsonCache
  , simpleJsonCache'
  , jsonCache
  , jsonCache'

  -- ** Mustache
  , compileTemplate'

  -- ** Re-exported
  , module Text.Mustache
  ) where

import           Slick.Build
import           Slick.Caching
import           Slick.Mustache
import           Slick.Pandoc
import           Text.Mustache  hiding ((~>))
