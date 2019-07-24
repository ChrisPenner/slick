{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Slick.Caching
  ( simpleJsonCache
  , simpleJsonCache'
  , jsonCache
  , jsonCache'
  )
where

import           Data.Aeson                    as A
import           Data.ByteString.Lazy
import           Development.Shake                 hiding ( Resource )
import           Development.Shake.Classes
import           GHC.Generics                             ( Generic )


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
