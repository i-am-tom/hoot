{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Hoot.Contentful.Asset
Description : A data type for representing assets from Contentful.
Copyright   : (c) Tom Harding, 2020
License     : MIT
Maintainer  : i.am.tom.harding@gmail.com
Stability   : experimental
-}
module Hoot.Contentful.Asset where

import Data.Aeson ((.:), FromJSON (..), Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | An @Asset@ as represented by Contentful's API. An asset may be any file
-- type, and is entirely up to your content schema within Contentful. The
-- content type and file name are provided, though they're not especially
-- useful; because Mustache doesn't allow logic within templates, you can't
-- perform any conditional checks based on the information.
data Asset
  = Asset
      { title       :: Text  -- ^ User-specified asset title.
      , contentType :: Text  -- ^ Asset content type, such as @image/jpeg@.
      , description :: Text  -- ^ User-specified asset description.
      , details     :: Value -- ^ Asset details (e.g. image dimensions).
      , fileName    :: Text  -- ^ The filename of the asset on Contentful.
      , url         :: Text  -- ^ The URL of the asset on Contentful's CDNs.
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance FromJSON Asset where
  parseJSON :: Value -> Parser Asset
  parseJSON = Aeson.withObject "Asset" \asset -> do
    asset .: "fields" >>= Aeson.withObject "fields" \fields -> do
      title       <- fields .: "title"
      description <- fields .: "description"

      fields .: "file" >>= Aeson.withObject "file" \file -> do
        contentType <- file .: "contentType"
        details     <- file .: "details"
        fileName    <- file .: "fileName"
        url         <- file .: "file"

        pure Asset{..}
