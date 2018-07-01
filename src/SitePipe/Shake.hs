{-# LANGUAGE DeriveFunctor #-}

module SitePipe.Shake where

import Data.Aeson
import Data.Map
import qualified Data.Text as T
import Development.Shake hiding (Resource)
import Text.Pandoc
import Text.Pandoc.Shared

markdownOptions :: ReaderOptions
markdownOptions =
  def {readerExtensions = extensionsFromList [Ext_yaml_metadata_block]}

-- resourceLoader' ::
--      (FromJSON v) => (String -> Action String) -> [FilePath] -> Action [v]
-- resourceLoader' fileReader fileNames = do
--   traverse (loadWith fileReader) filenames
-- | loads a file from filepath and applies a given filereader.
loadWith ::
     (FromJSON a)
  => (String -> Action a) -- Reader
  -> FilePath
  -> Action a
loadWith fileReader filePath = do
  file <- readFile' filePath
  fileReader file

unPandocM :: PandocPure a -> Action a
unPandocM = either (fail . show) return . runPure

markdownReader :: (FromJSON a) => String -> Action a
markdownReader file = do
  pdoc@(Pandoc (Meta meta) _) <-
    unPandocM $ readMarkdown markdownOptions (T.pack file)
  htmlString <- unPandocM $ writeHtml5String def pdoc
  liftIO $ print $ "SOURCE:" ++ (file)
  liftIO $ print $ "META:" ++ show (flattenMeta (Meta meta))
  let withContent = insert "content" (MetaString $ T.unpack htmlString) (meta)
  case fromJSON $ flattenMeta (Meta withContent) of
    Success r -> pure r
    Error err -> fail $ "error in markdownReader:" ++ err

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
