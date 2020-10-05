{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Hoot.Contentful.ContentType
Description : Type definitions for Contentful's content types.
Copyright   : (c) Tom Harding, 2020
License     : MIT
Maintainer  : i.am.tom.harding@gmail.com
Stability   : experimental
-}
module Hoot.Contentful.ContentType where

import Data.Aeson ((.:), FromJSON (..), Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Hoot.Contentful.Id (Catalogue (..), Id)
import Prelude hiding (id)

-- | A content type represents a schema for content entries, as well as some
-- metadata. We can use the @displayField@ property for displaying errors to
-- the user in a way that matches the displayed information in the Contentful
-- user interface.
data ContentType
  = ContentType
      { name         :: Text            -- ^ The name defined in Contentful.
      , description  :: Maybe Text      -- ^ The optional description.
      , displayField :: Text            -- ^ The field used for an entry title.
      , fields       :: Catalogue Field -- ^ The field types.
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance FromJSON ContentType where
  parseJSON :: Value -> Parser ContentType
  parseJSON = Aeson.withObject "ContentType" \obj -> do
    name         <- obj .: "name"
    description  <- obj .: "description"
    displayField <- obj .: "displayField"

    let parseIdentifier :: Value -> Parser (Id Field)
        parseIdentifier = Aeson.withObject "field" \field -> field .: "id"

    pairs <- obj .: "fields" >>= traverse \json -> do
      field <- parseJSON json
      id    <- parseIdentifier json

      pure ( id, field )

    let fields = Catalogue (HashMap.fromList pairs)
    pure ContentType{..}

-- | The type of nested Contentful links.
data Link
  = Asset -- ^ A link to an asset (such as an image).
  | Entry -- ^ A link to another piece of content (to nest within this one).
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, Hashable)

-- | The type of values that are allowed in a field.
data Type
  = Array Type -- ^ Some number of values of the same type.
  | Boolean    -- ^ A boolean value.
  | Date       -- ^ An ISO 8601 date string.
  | Integer    -- ^ An integer value.
  | Link Link  -- ^ A link to another Contentful entry.
  | Location   -- ^ A @{ lat, lon }@ object.
  | Number     -- ^ A floating-point value.
  | Object     -- ^ A JSON object.
  | Symbol     -- ^ A short string of text.
  | Text       -- ^ A long string of text.
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

-- | A field defines the attributes for a value within a content type.
data Field
  = Field
      { name      :: Text -- ^ The name of this field.
      , value     :: Type -- ^ The type of this field's value.
      , disabled  :: Bool -- ^ Should we ignore this field's content?
      , localized :: Bool -- ^ Has this string been localised?
      , omitted   :: Bool -- ^ Should this field be ommitted?
      , required  :: Bool -- ^ Is this field required in the UI?
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance FromJSON Field where
  parseJSON :: Value -> Parser Field
  parseJSON = Aeson.withObject "Field" \field -> do
    let parseType :: HashMap Text Value -> Parser Type
        parseType obj = obj .: "type" >>= Aeson.withText "type" \case
          "Array"    -> obj .: "items" >>= fmap Array . parseType
          "Boolean"  -> pure Boolean
          "Date"     -> pure Date
          "Integer"  -> pure Integer
          "Link"     -> fmap Link (obj .: "linkType")
          "Location" -> pure Location
          "Number"   -> pure Number
          "Object"   -> pure Object
          "Symbol"   -> pure Symbol
          "Text"     -> pure Text
          unknown    -> fail ("Unexpected type: " <> Text.unpack unknown)

    disabled  <- field .: "disabled"
    localized <- field .: "localized"
    name      <- field .: "name"
    omitted   <- field .: "omitted"
    required  <- field .: "required"
    value     <- parseType field

    pure Field{..}
