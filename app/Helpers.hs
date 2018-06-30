{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers where

import Data.Aeson as A
import Data.ByteString.Lazy
import Development.Shake hiding (Resource)
import Development.Shake.Classes
import GHC.Generics (Generic)

newtype CacheQuery q =
  CacheQuery q
  deriving (Show, Eq, Generic, Binary, NFData, Hashable)

jsonOracle ::
     forall a q. (ToJSON a, FromJSON a, ShakeValue q)
  => (q -> Action a)
  -> Rules (q -> Action a)
jsonOracle loader =
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

type instance RuleResult (CacheQuery q) = ByteString

simpleJsonCache ::
     (ToJSON a, FromJSON a)
  => (String -> Action a)
  -> Rules (String -> Action a)
simpleJsonCache = jsonOracle
