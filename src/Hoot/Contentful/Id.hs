{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Hoot.Contentful.Id
Description : A data type for Contentful IDs.
Copyright   : (c) Tom Harding, 2020
License     : MIT
Maintainer  : i.am.tom.harding@gmail.com
Stability   : experimental
-}
module Hoot.Contentful.Id where

import Data.Aeson (FromJSON (..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Prelude hiding (id)

-- | A Contentful ID is an identifier that we can use to reference entries or
-- assets. It is indexed by the type of entity that it references, to make sure
-- that they can't be mixed up.
newtype Id (entity :: Type)
  = Id { toText :: Text }
  deriving stock (Eq, Generic, Ord)
  deriving newtype (FromJSON, Hashable, IsString)

instance Show (Id x) where
  show = show . Text.unpack . toText

-- | A Rolodex is a 'HashMap' of entities indexed by an identifier. Naming
-- things is the hardest problem in computer science.
type Rolodex (x :: Type)
  = HashMap (Id x) x

-- | Given a way to extract an identifier, turn a list of items into a map of
-- values indexed by that identifier.
index :: (x -> Id x) -> [x] -> HashMap (Id x) x
index f = HashMap.fromList . map \x -> (f x, x)
