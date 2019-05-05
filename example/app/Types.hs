{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (liftM, mzero)
import           Data.Aeson                 as A
import           Data.Aeson.Encoding        (string)
import           Data.Aeson.Lens
import           Data.Aeson.TH
import           Data.Aeson.Types           (typeMismatch)
import           Data.Char
import           Data.Function              (on)
import           Data.List                  (intersperse, sortBy)
import           Data.Map                   as M
import           Data.Monoid
import           Data.Ord                   (comparing)
import           Data.Set                   as S
import           Data.String
import qualified Data.Text                  as T
import           Data.Text.Lens
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)


-------------------------------------------------------------------------------
-- Core Data Types and Instances

-- | Represents the template dependencies of the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | A JSON serializable representation of a post's metadata
data Post =
  Post
    { title   :: String
    , author  :: String
    , content :: String
    , url     :: String
    , date    :: String
    , image   :: Maybe String
    } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

-- | Generalized version of filepath
newtype EntityFilePath a =
  EntityFilePath String
   deriving (Show, Eq,  Generic, Hashable, Binary, NFData)

-- | A simple wrapper data-type which implements 'ShakeValue';
--   Used as a Shake Cache key to build a cache of post objects.
newtype PostFilePath =
  PostFilePath String
  deriving (Show, Eq, Generic, Hashable, Binary, NFData)
