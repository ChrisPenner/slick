{-|
Module      : Slick
Description : A quick & simple static site builder built on Shake and Pandoc
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}
module Slick
  (
  -- * Slick
  -- | This module re-exports the basics you need to run slick.
  --  For more complex tasks look into "Slick.Pandoc".
  --
  --  To get started use the <https://github.com/ChrisPenner/slick-template Slick Template>.

  -- ** Basics
    slick
  , slickWithOpts
  , markdownToHTML
  , markdownToHTML'
  , orgModeToHTML
  , orgModeToHTML'

  -- ** Mustache Templating
  , compileTemplate'
  , substitute

  -- ** Utils
  , getDirectoryPaths
  , convert
  )
where

import Slick.Mustache
import Slick.Pandoc
import Slick.Shake
import Slick.Utils
import Text.Mustache
