{-# LANGUAGE OverloadedStrings #-}

module Slick.Pandoc
  ( markdownToHTML
  , markdownToHTML'
  , makePandocReader
  , makePandocReader'
  , PandocReader
  , PandocWriter
  , loadUsing
  , loadUsing'
  , loadEntity
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text                  as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               hiding (Meta)
import           Text.Pandoc
import           Text.Pandoc.Shared

import           Slick.Build
import           Slick.Utils

--------------------------------------------------------------------------------

type PandocReader textType = textType -> PandocIO Pandoc
type PandocWriter = Pandoc -> PandocIO T.Text

--------------------------------------------------------------------------------

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result

-- | Convert markdown text into a 'Value';
--   The 'Value'  has a "content" key containing rendered HTML
--   Metadata is assigned on the respective keys in the 'Value'
markdownToHTML :: ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
               -> WriterOptions  -- ^ Pandoc writer options to modify output
               -> T.Text         -- ^ Text for conversion
               -> Action Value
markdownToHTML readOps writeOps textToConvert =
  loadUsing
    (readMarkdown readOps)
    (writeHtml5String writeOps)
    textToConvert

-- | Like 'markdownToHTML' but allows returning any JSON serializable object
markdownToHTML' :: (FromJSON a)
                => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
                -> WriterOptions  -- ^ Pandoc writer options to modify output
                -> T.Text         -- ^ Text for conversion
                -> Action a
markdownToHTML' rops wops txt =
  mseq (markdownToHTML rops wops) convert txt

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
  let withContent = meta & _Object . at "content" ?~ String outText
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

-- | Generalized Entity Loader, wrapper in
--   own function to respolve EntityFilePath properly
loadEntity :: FromJSON b
           => ReaderOptions      -- ^ Pandoc reader options to specify extensions or other functionality
           -> WriterOptions      -- ^ Pandoc writer options to modify output
           -> EntityFilePath a   -- ^ Path to entity ready for conversion, usually text file
           -> Action b
loadEntity rops wops (EntityFilePath path) =
  subloadEntity rops wops path

-- | Helper functiona for generalized `loadEntity`
--   provides conversion from source to output file format
subloadEntity :: FromJSON b
              => ReaderOptions  -- ^ Pandoc reader options to specify extensions or other functionality
              -> WriterOptions  -- ^ Pandoc writer options to modify output
              -> FilePath       -- ^ Path to the file ready for conversion
              -> Action b
subloadEntity rops wops entityPath = do
  let srcPath = destToSrc entityPath -<.> "md"
  entityData <- readFile' srcPath >>= markdownToHTML rops wops . T.pack
  let entityURL = T.pack  . srcToURL $ entityPath
      withURL = _Object . at "url"     ?~ String entityURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ entityData
