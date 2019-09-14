{-|
Module      : Slick.Pandoc
Description : Slick utilities for working with Pandoc
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}
{-# LANGUAGE OverloadedStrings #-}

module Slick.Pandoc
  ( markdownToHTML
  , markdownToHTML'
  , markdownToHTMLWithOpts
  , markdownToHTMLWithOpts' 
  , makePandocReader
  , makePandocReader'
  , PandocReader
  , PandocWriter
  , loadUsing
  , loadUsing'
  , defaultMarkdownOptions
  , defaultHtml5Options
  , convert
  , flattenMeta
  ) where

import Data.Aeson
import Development.Shake
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared
import Slick.Utils
import Data.HashMap.Strict as HM

import qualified Data.Text                  as T

--------------------------------------------------------------------------------

type PandocReader textType = textType -> PandocIO Pandoc
type PandocWriter = Pandoc -> PandocIO T.Text

-- | Reasonable options for reading a markdown file. Behaves similar to Github Flavoured
-- Markdown
defaultMarkdownOptions :: ReaderOptions
defaultMarkdownOptions =
  def { readerExtensions = exts }
  where
    exts = mconcat
     [ extensionsFromList
       [ Ext_yaml_metadata_block
       , Ext_fenced_code_attributes
       , Ext_auto_identifiers
       ]
     , githubMarkdownExtensions
     ]

-- | Reasonable options for rendering to HTML. Includes default code highlighting rules
defaultHtml5Options :: WriterOptions
defaultHtml5Options =
  def { writerHighlightStyle = Just tango
      , writerExtensions     = writerExtensions def
      }

--------------------------------------------------------------------------------

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result

-- | Convert markdown text into a 'Value';
--
--   The 'Value'  has a "content" key containing rendered HTML.
--
--   Metadata is assigned on the respective keys in the 'Value'
markdownToHTML :: T.Text
               -> Action Value
markdownToHTML txt =
    markdownToHTMLWithOpts defaultMarkdownOptions defaultHtml5Options txt

-- | Like 'markdownToHTML' but allows returning any JSON serializable object
markdownToHTML' :: (FromJSON a)
                => T.Text
                -> Action a
markdownToHTML' txt =
    markdownToHTML txt >>= convert

-- | Like 'markdownToHTML' but allows returning any JSON serializable object
markdownToHTMLWithOpts
    :: ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action Value
markdownToHTMLWithOpts rops wops txt =
  loadUsing
    (readMarkdown rops)
    (writeHtml5String wops)
    txt

-- | Like 'markdownToHTML' but allows returning any JSON serializable object
markdownToHTMLWithOpts'
    :: (FromJSON a)
    => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action a
markdownToHTMLWithOpts' rops wops txt =
    markdownToHTMLWithOpts rops wops txt >>= convert

-- | Given a reader from 'Text.Pandoc.Readers' this creates a loader which
--   given the source document will read its metadata into a 'Value'
--   returning both the 'Pandoc' object and the metadata within an 'Action'
makePandocReader :: PandocReader textType
                 -> textType
                 -> Action (Pandoc, Value)
makePandocReader readerFunc text = do
  pdoc@(Pandoc meta _) <- unPandocM $ readerFunc text
  return (pdoc, flattenMeta meta)

-- | Like 'makePandocReader' but will deserialize the metadata into any object
--   which implements 'FromJSON'. Failure to deserialize will fail the Shake
--   build.
makePandocReader' :: (FromJSON a) => PandocReader textType
                  -> textType
                  -> Action (Pandoc, a)
makePandocReader' readerFunc text = do
  (pdoc, meta)  <- makePandocReader readerFunc text
  convertedMeta <- convert meta
  return (pdoc, convertedMeta)

--------------------------------------------------------------------------------

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   into text using the given 'PandocWriter'.
--   Returns a 'Value' wherein the rendered text is set to the "content" key and
--   any metadata is set to its respective key in the 'Value'
loadUsing :: PandocReader textType
          -> PandocWriter
          -> textType
          -> Action Value
loadUsing reader writer text = do
  (pdoc, meta) <- makePandocReader reader text
  outText      <- unPandocM $ writer pdoc
  withContent <- case meta of
      Object m -> return . Object $ HM.insert "content" (String outText) m
          -- meta & _Object . at "content" ?~ String outText
      _ -> fail "Failed to parse metadata"
  return withContent

-- | Like 'loadUsing' but allows also deserializes the 'Value' into any object
--   which implements 'FromJSON'.  Failure to deserialize will fail the Shake
--   build.
loadUsing' :: (FromJSON a)
           => PandocReader textType
           -> PandocWriter
           -> textType
           -> Action a
loadUsing' reader writer text =
  loadUsing reader writer text >>= convert

--------------------------------------------------------------------------------

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
--   text objects into plain strings along the way.
flattenMeta :: Meta -> Value
flattenMeta (Meta meta) = toJSON $ fmap go meta
 where
  go :: MetaValue -> Value
  go (MetaMap     m) = toJSON $ fmap go m
  go (MetaList    m) = toJSONList $ fmap go m
  go (MetaBool    m) = toJSON m
  go (MetaString  m) = toJSON m
  go (MetaInlines m) = toJSON $ stringify m
  go (MetaBlocks  m) = toJSON $ stringify m
