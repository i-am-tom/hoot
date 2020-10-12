{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Hoot.Contentful.IdTest where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as Text
import Hoot.Contentful.Id (Id (..))
import Test.Tasty.QuickCheck (property)
import Test.Tasty.Hspec

spec_id :: Spec
spec_id = describe "Hoot.Contentful.Id" do
  it "Parses a single Identifier" $ property \(Text.pack -> x) ->
    parseMaybe parseJSON (String x) `shouldBe` Just (Id x)
