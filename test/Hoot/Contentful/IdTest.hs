{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hoot.Contentful.IdTest where

import Data.Aeson ((.=), FromJSON (..), Value (..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Hoot.Contentful.Id (Catalogue (..), Id (..), Identified (..))
import Prelude hiding (head, id)
import Test.Tasty.QuickCheck (property)
import Test.Tasty.Hspec
import qualified Data.Aeson as Aeson

spec_id :: Spec
spec_id = describe "Hoot.Contentful.Id" do
  it "Parses a single Identifier" $ property \(Text.pack -> x) ->
    parseMaybe parseJSON (String x) `shouldBe`
      Just (Id x)

  it "Parses an identified entity" $ property \(Text.pack -> id) -> do
    let entity :: Value
        entity = Aeson.object [ "sys" .= Aeson.object [ "id" .= id ] ]

    parseMaybe parseJSON entity `shouldBe`
      Just Identified{ id = Id id, entity = entity }

  it "Parses a Catalogue" $ property \(Text.pack -> id) -> do
    let entity :: Value
        entity = Aeson.object [ "sys" .= Aeson.object [ "id" .= id ] ]

    parseMaybe parseJSON (Aeson.Array [ entity ]) `shouldBe`
      Just (Catalogue (HashMap.singleton (Id id) entity))
