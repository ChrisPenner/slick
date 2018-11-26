module Slick.Mustache
  ( compileTemplate'
  )
where

import           Development.Shake
import           Text.Mustache
import           Text.Mustache.Compile


-- | Like 'compileTemplate' but tracks changes to template files and partials
-- within Shake.
compileTemplate' :: FilePath -> Action Template
compileTemplate' fp = do
  need [fp]
  result <- liftIO $ localAutomaticCompile fp
  case result of
    Right templ -> do
      need (getPartials . ast $ templ)
      return templ
    Left err -> fail $ show err
