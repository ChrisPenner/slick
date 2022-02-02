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
  , orgModeToHTML
  , orgModeToHTML'
  , orgModeToHTMLWithOpts
  , orgModeToHTMLWithOpts'
  , makePandocReader
  , makePandocReader'
  , makePandocReaderWithMetaWriter
  , makePandocReaderWithMetaWriter'
  , PandocReader
  , PandocWriter
  , loadUsing
  , loadUsing'
  , loadUsingMeta
  , defaultMarkdownOptions
  , defaultOrgModeOptions
  , defaultHtml5Options
  , convert
  , flattenMeta
  ) where

import Data.Aeson
import Data.Aeson.KeyMap as KM
import Development.Shake
import Text.Pandoc
import Text.Pandoc.Highlighting
import Slick.Utils

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

-- | Reasonable options for reading an org-mode file
defaultOrgModeOptions :: ReaderOptions
defaultOrgModeOptions =
  def { readerExtensions = exts }
  where
    exts = mconcat
     [ extensionsFromList
       [ Ext_fenced_code_attributes
       , Ext_auto_identifiers
       ]
     ]
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

-- | Like 'markdownToHTML' but allows providing additional pandoc reader and writer options
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

-- | Like 'markdownToHTMLWithOpts' but returns any JSON serializable object.
markdownToHTMLWithOpts'
    :: (FromJSON a)
    => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action a
markdownToHTMLWithOpts' rops wops txt =
    markdownToHTMLWithOpts rops wops txt >>= convert

-- | Convert org-mode text into a 'Value';
--
--   The 'Value'  has a "content" key containing rendered HTML.
--
--   Metadata is assigned on the respective keys in the 'Value'
orgModeToHTML :: T.Text
               -> Action Value
orgModeToHTML txt =
    orgModeToHTMLWithOpts defaultOrgModeOptions defaultHtml5Options txt

-- | Like 'orgModeToHTML' but allows returning any JSON compatible object.
orgModeToHTML' :: (FromJSON a)
               => T.Text
               -> Action a
orgModeToHTML' txt =
    orgModeToHTML txt >>= convert

-- | Like 'orgModeToHTML' but allows providing additional pandoc reader and writer options
orgModeToHTMLWithOpts
    :: ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action Value
orgModeToHTMLWithOpts rops wops txt =
  loadUsing
    (readOrg rops)
    (writeHtml5String wops)
    txt

-- | Like 'orgModeToHTMLWithOpts' but allows returning any JSON compatible object
orgModeToHTMLWithOpts'
    :: (FromJSON a)
    => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
    -> WriterOptions  -- ^ Pandoc writer options to modify output
    -> T.Text         -- ^ Text for conversion
    -> Action a
orgModeToHTMLWithOpts' rops wops txt =
    orgModeToHTMLWithOpts rops wops txt >>= convert

-- | Given a reader from 'Text.Pandoc.Readers' this creates a loader which
--   given the source document will read its metadata into a 'Value'
--   returning both the 'Pandoc' object and the metadata within an 'Action'.
--   The metadata values will be read as Markdown but rendered as plain text,
--   removing any links, pictures, and inline formatting.
makePandocReader :: PandocReader textType
                 -> textType
                 -> Action (Pandoc, Value)
makePandocReader readerFunc text =
  makePandocReaderWithMetaWriter readerFunc (writePlain def) text

-- | Given a reader from 'Text.Pandoc.Readers', and a writer from
--   'Text.Pandoc.Writers', this creates a loader which given the source
--   document will read its metadata as Markdown, then render it into a
--   'Value' using the writer, returning both the 'Pandoc' object and the
--   metadata within an 'Action'
makePandocReaderWithMetaWriter
    :: PandocReader textType
    -> PandocWriter
    -> textType
    -> Action (Pandoc, Value)
makePandocReaderWithMetaWriter readerFunc writerFunc text = do
  pdoc@(Pandoc meta _) <- unPandocM $ readerFunc text
  meta' <- flattenMeta writerFunc meta
  return (pdoc, meta')

-- | Like 'makePandocReader' but will deserialize the metadata
--   into any object which implements 'FromJSON'. Failure to deserialize will
--   fail the Shake build. Metadata values will be read as Markdown but
--   rendered as plain text, removing any links, pictures, and inline
--   formatting.
makePandocReader'
    :: (FromJSON a)
    => PandocReader textType
    -> textType
    -> Action (Pandoc, a)
makePandocReader' readerFunc text =
  makePandocReaderWithMetaWriter' readerFunc (writePlain def) text

-- | Like 'makePandocReaderWithMetaWriter' but will deserialize the metadata
--   into any object which implements 'FromJSON'. Failure to deserialize will
--   fail the Shake build.
makePandocReaderWithMetaWriter'
    :: (FromJSON a)
    => PandocReader textType
    -> PandocWriter
    -> textType
    -> Action (Pandoc, a)
makePandocReaderWithMetaWriter' readerFunc writerFunc text = do
  (pdoc, meta)  <- makePandocReaderWithMetaWriter readerFunc writerFunc text
  convertedMeta <- convert meta
  return (pdoc, convertedMeta)

--------------------------------------------------------------------------------

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   into text using the given 'PandocWriter'. Takes a second 'PandocWriter' to render
--   metadata.
--   Returns a 'Value' wherein the rendered text is set to the "content" key and
--   any metadata is set to its respective key in the 'Value'
loadUsingMeta :: PandocReader textType -- ^ The reader used to load the document
          -> PandocWriter -- ^ The writer used to render the document itself
          -> PandocWriter -- ^ The writer used to process metadata.
          -> textType
          -> Action Value
loadUsingMeta reader writer metaWriter text = do
  (pdoc, meta) <- makePandocReaderWithMetaWriter reader metaWriter text
  outText      <- unPandocM $ writer pdoc
  withContent <- case meta of
      Object m -> return . Object $ KM.insert "content" (String outText) m
          -- meta & _Object . at "content" ?~ String outText
      _ -> fail "Failed to parse metadata"
  return withContent

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   into text using the given 'PandocWriter'.
--   Returns a 'Value' wherein the rendered text is set to the "content" key and
--   any metadata is set to its respective key in the 'Value'
loadUsing :: PandocReader textType
          -> PandocWriter
          -> textType
          -> Action Value
loadUsing reader writer text = loadUsingMeta reader writer writer text

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
flattenMeta :: PandocWriter -> Meta -> Action Value
flattenMeta writer (Meta meta) = toJSON <$> traverse go meta
 where
  go :: MetaValue -> Action Value
  go (MetaMap     m) = toJSON <$> traverse go m
  go (MetaList    m) = toJSONList <$> traverse go m
  go (MetaBool    m) = pure $ toJSON m
  go (MetaString  m) = pure $ toJSON m
  go (MetaInlines m) = toJSON <$> (unPandocM . writer . Pandoc mempty . (:[]) . Plain $ m)
  go (MetaBlocks  m) = toJSON <$> (unPandocM . writer . Pandoc mempty $ m)
