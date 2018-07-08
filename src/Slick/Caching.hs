{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Slick.Caching
  ( simpleJsonCache
  , jsonCache
  ) where

import Data.Aeson as A
import Data.ByteString.Lazy
import Development.Shake hiding (Resource)
import Development.Shake.Classes
import GHC.Generics (Generic)

newtype CacheQuery q =
  CacheQuery q
  deriving (Show, Eq, Generic, Binary, NFData, Hashable)

jsonCache ::
     forall a q. (ToJSON a, FromJSON a, ShakeValue q)
  => (q -> Action a)
  -> Rules (q -> Action a)
jsonCache loader =
  unpackJSON <$> addOracleCache (\(CacheQuery q) -> A.encode <$> loader q)
  where
    unpackJSON ::
         FromJSON a => (CacheQuery q -> Action ByteString) -> q -> Action a
    unpackJSON runCacheQuery =
      \q -> do
        bytes <- runCacheQuery $ CacheQuery q
        case A.eitherDecode bytes of
          Left err -> fail err
          Right res -> pure res

simpleJsonCache ::
     forall q a. (ToJSON a, FromJSON a, ShakeValue q)
  => q
  -> Action a
  -> Rules (Action a)
simpleJsonCache q loader = do
  cacheGetter <- jsonCache (const loader)
  return $ cacheGetter q

type instance RuleResult (CacheQuery q) = ByteString
