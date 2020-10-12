{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Hoot.Contentful.Entry
Description : Type definitions for Contentful's entries.
Copyright   : (c) Tom Harding, 2020
License     : MIT
Maintainer  : i.am.tom.harding@gmail.com
Stability   : experimental
-}
module Hoot.Contentful.Entry where

import Data.Aeson ((.:), FromJSON (..), Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Extra (U (..))
import Data.Aeson.Types (Parser, parseFail)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Hoot.Contentful.Asset (Asset)
import qualified Hoot.Contentful.ContentType as Model
import Hoot.Contentful.Id (Id (..), Rolodex)
import Prelude hiding (id)

-- | An entry is a map from field names to their contents, with the type of the
-- contents being defined by a 'Model.ContentType' schema.
data Entry
  = Entry
      { id     :: Id Entry
      , fields :: HashMap Text Content
      }
  deriving (Eq, Ord, Show)

-- | The content of an entry's field has a type corresponding to a value of
-- 'T.Type'. The schema is important, as the representations of these types
-- aren't mutually exclusive: for example, a 'Model.Link' will always parse as
-- an 'Object', but we need to know that it's a 'Model.Link' in order to
-- resolve it.
data Content
  = Array [Content]
  | Boolean Bool
  | Date UTCTime
  | Integer Scientific
  | Media (Id Asset)
  | Location { lat :: Scientific, lon :: Scientific }
  | Number Scientific
  | Object Value
  | Reference (Id Entry)
  | Symbol Text
  | Text Text
  | Null -- ^ This value was optional and/or omitted.
  deriving stock (Eq, Generic, Ord, Show)

-- | Parse an 'Entry' 'Model.field' according to a 'Model.Type' definition.
-- This function could be recursive if the value is expected to be an 'Array'
-- of a given 'Model.Type'.
parseValue :: Model.Type -> Value -> Parser Content
parseValue = \case
  Model.Boolean -> Aeson.withBool "Boolean" (pure . Boolean)
  Model.Date    -> fmap (Date . getU) . parseJSON
  Model.Integer -> Aeson.withScientific "Integer" (pure . Integer)
  Model.Number  -> Aeson.withScientific "Integer" (pure . Number)
  Model.Object  -> pure . Object
  Model.Symbol  -> Aeson.withText "Symbol" (pure . Symbol)
  Model.Text    -> Aeson.withText "Text" (pure . Text)

  Model.Array inner -> Aeson.withArray "Array" \(toList -> xs) -> do
    pieces <- traverse (parseValue inner) xs
    pure (Array pieces)

  Model.Link Model.Asset -> Aeson.withObject "Asset" \obj ->
    obj .: "sys" >>= Aeson.withObject "sys" \sys -> do
      "Link"  <- sys .: "type"     >>= Aeson.withText "type"     pure
      "Asset" <- sys .: "linkType" >>= Aeson.withText "linkType" pure

      fmap Media (sys .: "id")

  Model.Link Model.Entry -> Aeson.withObject "Entry" \obj ->
    obj .: "sys" >>= Aeson.withObject "sys" \sys -> do
      "Link"  <- sys .: "type"     >>= Aeson.withText "type"     pure
      "Entry" <- sys .: "linkType" >>= Aeson.withText "linkType" pure

      fmap Media (sys .: "id")

  Model.Location -> Aeson.withObject "Location" \obj -> do
    lat <- obj .: "lat"
    lon <- obj .: "lon"

    pure Location{..}

-- | Given an unparsed 'Entry', figure out its 'Model.ContentType', and then
-- parse it according to that 'Model.ContentType''s schema.
parseEntry :: Rolodex Model.ContentType -> Value -> Parser Entry
parseEntry schema = Aeson.withObject "Entry" \obj ->
  obj .: "sys" >>= Aeson.withObject "sys" \sys ->
    sys .: "contentType" >>= Aeson.withObject "contentType" \contentType ->
      contentType .: "sys" >>= Aeson.withObject "sys" \sys' -> do
        "Link"        <- sys' .: "type"     >>= Aeson.withText "type"     pure
        "ContentType" <- sys' .: "linkType" >>= Aeson.withText "linkType" pure

        -- We've found a content type, so we can parse the entry against this.
        Model.ContentType{ name = Text.unpack -> name, fields = specs } <-
          sys' .: "id" >>= \id ->
            case HashMap.lookup id schema of
              Just result -> pure result
              Nothing     -> parseFail ("Unknown content type: " <> show contentType)

        fields <- obj .: "fields" >>= Aeson.withObject name \unparsed ->
          specs & HashMap.traverseWithKey \key Model.Field{ value, required } ->
            case HashMap.lookup key unparsed of
              Just json -> parseValue value json
              Nothing
                | required  -> parseFail ("Missing required key: " <> Text.unpack key)
                | otherwise -> pure Null

        id <- sys .: "id"
        pure Entry{..}
