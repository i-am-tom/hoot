{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hoot.Contentful.Id where

import Data.Aeson ((.:), FromJSON (..), Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

-- | A Contentful ID is an identifier that we can use to reference entries or
-- assets. It is indexed by the type of entity that it references, to make sure
-- that they can't be mixed up.
newtype Id (entity :: Type)
  = Id { toText :: Text }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, Hashable, IsString)

-- | An /identified/ entity is an entity that has an identifier. Specifically,
-- we expect the identifier to be in the entity's JSON under @.sys.id@.
data Identified (entity :: Type)
  = Identified
      { id     :: Id entity
      , entity :: entity
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

-- | Convert an 'Identified' entity into a pair of its identifier and the
-- entity itself. This mostly exists for internal use in 'parseJSON', and
-- probably isn't useful anywhere else.
toPair :: Identified x -> (Id x, x)
toPair Identified{..} = ( id, entity )

instance FromJSON x => FromJSON (Identified x) where
  parseJSON :: Value -> Parser (Identified x)
  parseJSON json = do
    let parseIdentifier :: Value -> Parser (Id entity)
        parseIdentifier = Aeson.withObject "Entity" \asset ->
          asset .: "sys" >>= Aeson.withObject "sys" \sys ->
            sys .: "id"

    id     <- parseIdentifier json
    entity <- parseJSON json

    pure Identified{..}

-- | An entity catalogue is a 'HashMap' in which the keys are 'Id' references
-- to their entity values. For our purposes, entity catalogues are likely to be
-- filled with @Entry@, @Asset@, and @ContentType@ values.
newtype Catalogue (entity :: Type)
  = Catalogue { toHashMap :: HashMap (Id entity) entity }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

instance FromJSON x => FromJSON (Catalogue x) where
  parseJSON :: Value -> Parser (Catalogue x)
  parseJSON = fmap (Catalogue . HashMap.fromList . map toPair) . parseJSON
