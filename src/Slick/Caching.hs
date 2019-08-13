{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Slick.Caching
  ( simpleJsonCache
  , simpleJsonCache'
  , jsonCache
  , jsonCache'
  , shakeArgsAlwaysPruneWith
  , pruner
  )
where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.ByteString.Lazy
import           Data.List                  as L (intersperse, sortBy, (\\))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Development.Shake          hiding (Resource)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           System.Console.GetOpt
import           System.Directory.Extra
import           System.IO.Extra            as IO

import           Slick.Utils

--------------------------------------------------------------------------------

newtype CacheQuery q =
  CacheQuery q
  deriving (Show, Eq, Generic, Binary, NFData, Hashable)

type instance RuleResult (CacheQuery q) = ByteString

-- | A wrapper around 'addOracleCache' which given a @q@ which is a 'ShakeValue'
-- allows caching and retrieving 'Value's within Shake. See documentation on
-- 'addOracleCache' or see Slick examples for more info.
--
-- > -- We need to define a unique datatype as our cache key
-- > newtype PostFilePath =
-- >   PostFilePath String
-- > -- We can derive the classes we need (using GeneralizedNewtypeDeriving)
-- > -- so long as the underlying type implements them
-- >   deriving (Show, Eq, Hashable, Binary, NFData)
-- > -- now in our shake rules we can create a cache by providing a loader action
-- >
-- > do
-- > postCache <- jsonCache $ \(PostFilePath path) ->
-- >   readFile' path >>= markdownToHTML . Text.pack
-- > -- Now use postCache inside an Action to load your post with caching!
jsonCache :: ShakeValue q => (q -> Action Value) -> Rules (q -> Action Value)
jsonCache = jsonCache'

-- | Like 'jsonCache' but allows caching/retrieving any JSON serializable
-- objects.
jsonCache'
  :: forall a q
   . (ToJSON a, FromJSON a, ShakeValue q)
  => (q -> Action a)
  -> Rules (q -> Action a)
jsonCache' loader = unpackJSON
  <$> addOracleCache (\(CacheQuery q) -> A.encode <$> loader q)
 where
  unpackJSON
    :: FromJSON a => (CacheQuery q -> Action ByteString) -> q -> Action a
  unpackJSON runCacheQuery = \q -> do
    bytes <- runCacheQuery $ CacheQuery q
    case A.eitherDecode bytes of
      Left  err -> fail err
      Right res -> pure res

-- | A wrapper around 'jsonCache' which simplifies caching of values which do NOT
-- depend on an input parameter. Unfortunately Shake still requires that the
-- key type implement several typeclasses, however this is easily accomplished
-- using @GeneralizedNewtypeDeriving@ and a wrapper around @()@.
-- example usage:
--
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > module Main where
-- > newtype ProjectList = ProjectList ()
-- >   deriving (Show, Eq, Hashable, Binary, NFData)
--  Within your shake Rules:
--
-- > projectCache = simpleJsonCache (ProjectList ()) $ do
-- >   -- load your project list here; returning it as a Value
simpleJsonCache :: ShakeValue q => q -> Action Value -> Rules (Action Value)
simpleJsonCache = simpleJsonCache'

-- | Like 'simpleJsonCache' but allows caching any JSON serializable object.
simpleJsonCache'
  :: forall q a
   . (ToJSON a, FromJSON a, ShakeValue q)
  => q
  -> Action a
  -> Rules (Action a)
simpleJsonCache' q loader = do
  cacheGetter <- jsonCache' (const loader)
  return $ cacheGetter q

--------------------------------------------------------------------------------

-- | A version of 'shakeArgsPrunWith' that always do prune at the end.
shakeArgsAlwaysPruneWith :: ShakeOptions
                         -> ([FilePath] -> IO ())
                         -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ())))
                         -> IO ()
shakeArgsAlwaysPruneWith opts prune flags act = do
  let flags2 = fmap (fmapFmapOptDescr Just) flags
  IO.withTempFile $ \file -> do
    shakeArgsWith opts { shakeLiveFiles = file : shakeLiveFiles opts } flags2 $
      \opts args ->
        act (catMaybes opts) args
    src <- lines <$> IO.readFile' file
    prune src

-- | Remove all files that are in the target directory,
--   but not in the `shake` rules.
pruner :: FilePath -> [FilePath] -> IO ()
pruner root listOfFiles = do
  present <- listFilesRecursive root
  let listOfFilesToRemove = (toStandard <$> present) L.\\ (toStandard <$> listOfFiles)
      removedFiles = show listOfFilesToRemove
  mapM_ removeFile listOfFilesToRemove

  Prelude.putStrLn $ "Pruned stale outputs: " <> removedFiles
