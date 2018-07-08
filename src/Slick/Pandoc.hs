{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Slick.Pandoc
  ( markdownToHTML
  , markdownToHTML'
  , makePandocReader
  , makePandocReader'
  , loadUsing
  , loadUsing'
  , convert
  ) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import Development.Shake hiding (Resource)
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared

markdownOptions :: ReaderOptions
markdownOptions = def {readerExtensions = exts}
  where
    exts =
      mconcat
        [extensionsFromList [Ext_yaml_metadata_block], githubMarkdownExtensions]

html5Options :: WriterOptions
html5Options = def {writerHighlightStyle = Just tango}

unPandocM :: PandocPure a -> Action a
unPandocM = either (fail . show) return . runPure

markdownToHTML :: T.Text -> Action Value
markdownToHTML =
  loadUsing (readMarkdown markdownOptions) (writeHtml5String html5Options)

markdownToHTML' :: (FromJSON a) => T.Text -> Action a
markdownToHTML' = markdownToHTML >=> convert

type PandocReader = T.Text -> PandocPure Pandoc

type PandocWriter = Pandoc -> PandocPure T.Text

makePandocReader :: PandocReader -> T.Text -> Action (Pandoc, Value)
makePandocReader readerFunc text = do
  pdoc@(Pandoc meta _) <- unPandocM $ readerFunc text
  return (pdoc, flattenMeta meta)

makePandocReader' ::
     (FromJSON a)
  => (T.Text -> PandocPure Pandoc)
  -> T.Text
  -> Action (Pandoc, a)
makePandocReader' readerFunc text = do
  (pdoc, meta) <- makePandocReader readerFunc text
  convertedMeta <- convert meta
  return (pdoc, convertedMeta)

loadUsing :: PandocReader -> PandocWriter -> T.Text -> Action Value
loadUsing reader writer text = do
  (pdoc, meta) <- makePandocReader reader text
  outText <- unPandocM $ writer pdoc
  let withContent = meta & _Object . at "content" ?~ String outText
  return withContent

loadUsing' :: (FromJSON a) => PandocReader -> PandocWriter -> T.Text -> Action a
loadUsing' reader writer text = loadUsing reader writer text >>= convert

convert :: (FromJSON a, ToJSON a, FromJSON b) => a -> Action b
convert a =
  case fromJSON (toJSON a) of
    Success r -> pure r
    Error err -> fail $ "json conversion error:" ++ err

flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
  where
    go :: MetaValue -> Value
    go (MetaMap m) = toJSON $ fmap go m
    go (MetaList m) = toJSONList $ fmap go m
    go (MetaBool m) = toJSON m
    go (MetaString m) = toJSON m
    go (MetaInlines m) = toJSON $ stringify m
    go (MetaBlocks m) = toJSON $ stringify m
