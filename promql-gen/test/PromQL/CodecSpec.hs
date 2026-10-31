{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PromQL.CodecSpec (spec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Either (isLeft)
import PromQL
import PromQL.Codec ()
import PromQL.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Duration" $ do
    it "decodes what it encodes" $
      forAllValid $ \(duration :: Duration) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec duration) `shouldBe` Right duration
    it "is written as PromQL writes it" $
      toJSONViaCodec (Minutes 5) `shouldBe` JSON.String "5m"
    it "refuses a duration Prometheus has nowhere to put" $
      isLeft (JSON.parseEither (parseJSONViaCodec @Duration) (JSON.String "293y")) `shouldBe` True

  describe "LabelName" $ do
    it "decodes what it encodes" $
      forAllValid $ \(name :: LabelName) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec name) `shouldBe` Right name
    it "refuses a name Prometheus would not read as one" $
      isLeft (JSON.parseEither (parseJSONViaCodec @LabelName) (JSON.String "0a")) `shouldBe` True

  describe "MetricName" $ do
    it "decodes what it encodes" $
      forAllValid $ \(name :: MetricName) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec name) `shouldBe` Right name
    it "refuses a name Prometheus would not read as one" $
      isLeft (JSON.parseEither (parseJSONViaCodec @MetricName) (JSON.String "0a")) `shouldBe` True

  describe "Expr" $ do
    -- An expression decodes as one written the same way, which is the most
    -- that can be asked: some expressions are written alike.
    it "decodes what it encodes as an expression that encodes the same way" $
      forAllValid $ \(expr :: Expr) ->
        fmap (toJSONViaCodec @Expr) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec expr))
          `shouldBe` Right (toJSONViaCodec expr)
    it "is written as render writes it" $
      toJSONViaCodec (Binary Add OnEverything (Number 1) (Number 2)) `shouldBe` JSON.String "1 + 2"
    it "refuses an expression that is not valid" $
      isLeft (JSON.parseEither (parseJSONViaCodec @Expr) (JSON.String "NaN")) `shouldBe` True
