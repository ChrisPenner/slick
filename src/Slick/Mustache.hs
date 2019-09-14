{-|
Module      : Slick.Mustache
Description : Slick utilities for working with mustache
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}
module Slick.Mustache
  ( compileTemplate'
  )
where

import           Development.Shake
import           Text.Mustache
import           Text.Mustache.Compile

-- | Like 'compileTemplate' from <http://hackage.haskell.org/package/mustache mustache> but tracks changes to template files and partials within Shake for cache-busting.
compileTemplate' :: FilePath -> Action Template
compileTemplate' fp = do
  need [fp]
  result <- liftIO $ localAutomaticCompile fp
  case result of
    Right templ -> do
      need (getPartials . ast $ templ)
      return templ
    Left err -> fail $ show err
