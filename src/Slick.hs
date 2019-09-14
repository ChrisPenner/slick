module Slick
  (
  -- * Slick
  -- | This module re-exports the basics you need to run slick.
  -- You may need additional imports from "Slick.Pandoc" or "Slick.Caching".

  -- ** Basics
    markdownToHTML
  , markdownToHTML'
  , defaultMarkdownOptions
  , defaultHtml5Options
  , convert

  -- ** Mustache Templating
  , compileTemplate'
  , substitute

  -- ** Utils
  , getDirectoryPaths
  )
where

import Slick.Caching
import Slick.Mustache
import Slick.Pandoc
import Slick.Utils
import Text.Mustache
