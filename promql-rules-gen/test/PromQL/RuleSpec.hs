{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PromQL.RuleSpec (spec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Either (isLeft)
import qualified Data.Map as Map
import PromQL
import PromQL.Rule
import PromQL.Rule.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  -- Holding an expression, each of these decodes as one that encodes the
  -- same way, which is the most that can be asked: some expressions are
  -- written alike.
  describe "RuleFile" $ do
    genValidSpec @RuleFile
    it "decodes what it encodes as a file that encodes the same way" $
      forAllValid $ \(value :: RuleFile) ->
        fmap (toJSONViaCodec @RuleFile) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "RuleGroup" $ do
    genValidSpec @RuleGroup
    it "decodes what it encodes as a group that encodes the same way" $
      forAllValid $ \(value :: RuleGroup) ->
        fmap (toJSONViaCodec @RuleGroup) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "RuleGroupName" $ do
    genValidSpec @RuleGroupName
    it "decodes what it encodes" $
      forAllValid $ \(value :: RuleGroupName) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value
  describe "Rule" $ do
    genValidSpec @Rule
    it "decodes what it encodes as a rule that encodes the same way" $
      forAllValid $ \(value :: Rule) ->
        fmap (toJSONViaCodec @Rule) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "AlertingRule" $ do
    genValidSpec @AlertingRule
    it "decodes what it encodes as a rule that encodes the same way" $
      forAllValid $ \(value :: AlertingRule) ->
        fmap (toJSONViaCodec @AlertingRule) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "AlertName" $ do
    genValidSpec @AlertName
    it "decodes what it encodes" $
      forAllValid $ \(value :: AlertName) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value
  describe "RecordingRule" $ do
    genValidSpec @RecordingRule
    it "decodes what it encodes as a rule that encodes the same way" $
      forAllValid $ \(value :: RecordingRule) ->
        fmap (toJSONViaCodec @RecordingRule) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)

  -- Decoding asserts validity, so a file read in is one these types would
  -- have refused to build.
  describe "decoding" $ do
    let group name = JSON.object [("name", JSON.String name), ("rules", JSON.toJSON ([] :: [JSON.Value]))]
    it "refuses a file in which two groups have the same name" $
      isLeft (JSON.parseEither (parseJSONViaCodec @RuleFile) (JSON.object [("groups", JSON.toJSON [group "a", group "a"])]))
        `shouldBe` True
    it "refuses a rule that is neither an alert nor a recording" $
      isLeft (JSON.parseEither (parseJSONViaCodec @Rule) (JSON.object [("expr", "up")]))
        `shouldBe` True
    it "refuses a rule whose expression is not one" $
      isLeft (JSON.parseEither (parseJSONViaCodec @Rule) (JSON.object [("alert", "Down"), ("expr", "up +")]))
        `shouldBe` True
    it "reads a rule as Prometheus's documentation writes one" $
      JSON.parseEither
        (parseJSONViaCodec @Rule)
        ( JSON.object
            [ ("alert", "Down"),
              ("expr", "up == 0"),
              ("for", "5m"),
              ("labels", JSON.object [("severity", "page")])
            ]
        )
        `shouldBe` Right
          ( Alerting
              AlertingRule
                { alertingRuleName = AlertName "Down",
                  alertingRuleExpr = Binary isEqualTo OnEverything (Vector (metric (MetricName "up"))) (Number 0),
                  alertingRuleFor = Just (Minutes 5),
                  alertingRuleKeepFiringFor = Nothing,
                  alertingRuleLabels = Map.singleton (LabelName "severity") "page",
                  alertingRuleAnnotations = Map.empty
                }
          )

  -- What Prometheus refuses when it loads a file, and so what these types
  -- refuse to hold.
  describe "validity" $ do
    let up = Vector (metric (MetricName "up"))
    let group name =
          RuleGroup
            { ruleGroupName = RuleGroupName name,
              ruleGroupInterval = Nothing,
              ruleGroupRules = []
            }
    let alerting =
          AlertingRule
            { alertingRuleName = AlertName "Down",
              alertingRuleExpr = up,
              alertingRuleFor = Nothing,
              alertingRuleKeepFiringFor = Nothing,
              alertingRuleLabels = Map.empty,
              alertingRuleAnnotations = Map.empty
            }
    let recording =
          RecordingRule
            { recordingRuleName = MetricName "job:up:sum",
              recordingRuleExpr = up,
              recordingRuleLabels = Map.empty
            }

    it "takes a file whose groups all have different names" $
      isValid (RuleFile [group "a", group "b"]) `shouldBe` True

    it "refuses a file in which two groups have the same name" $
      isValid (RuleFile [group "a", group "a"]) `shouldBe` False

    it "refuses a file holding a group that is not valid" $
      isValid (RuleFile [group ""]) `shouldBe` False

    it "refuses a recording rule whose name is not a metric name" $
      isValid recording {recordingRuleName = MetricName "0a"} `shouldBe` False
    it "refuses a group or an alert without a name" $
      (isValid (group ""), isValid alerting {alertingRuleName = AlertName ""})
        `shouldBe` (False, False)

    it "takes an alert name with spaces in it" $
      isValid alerting {alertingRuleName = AlertName "NixCI production Down"}
        `shouldBe` True

    -- Nothing fills in a variable in a rule file, so one would reach
    -- Prometheus as it was written.
    it "refuses each duration of a rule that is a variable" $
      let variable = Just (DurationVariable "$__interval")
       in map
            isValid
            [ Alerting alerting {alertingRuleFor = variable},
              Alerting alerting {alertingRuleKeepFiringFor = variable},
              Alerting alerting
            ]
            `shouldBe` [False, False, True]

    it "refuses a group interval that is a variable" $
      isValid (group "a") {ruleGroupInterval = Just (DurationVariable "$__interval")}
        `shouldBe` False

    it "refuses a rule that sets the metric name as a label" $
      let labels = Map.singleton (LabelName "__name__") "x"
       in map
            isValid
            [ Alerting alerting {alertingRuleLabels = labels},
              Recording recording {recordingRuleLabels = labels}
            ]
            `shouldBe` [False, False]

    -- Annotations are not labels, so they may be called anything a label
    -- may be called.
    it "takes an annotation called __name__" $
      isValid alerting {alertingRuleAnnotations = Map.singleton (LabelName "__name__") "x"}
        `shouldBe` True

  describe "validateFixedDuration" $
    it "takes every unit of time and refuses a variable" $
      map
        (validationIsValid . validateFixedDuration)
        [ Milliseconds 1,
          Seconds 1,
          Minutes 1,
          Hours 1,
          Days 1,
          Weeks 1,
          Years 1,
          DurationVariable "$__interval"
        ]
        `shouldBe` [True, True, True, True, True, True, True, False]

  describe "renderRuleFile" $ do
    it "writes a file with one rule of each kind" $
      pureGoldenByteStringFile "test_resources/rule-file.json" $
        renderRuleFile
          RuleFile
            { ruleFileGroups =
                [ RuleGroup
                    { ruleGroupName = RuleGroupName "site",
                      ruleGroupInterval = Just (Seconds 30),
                      ruleGroupRules =
                        [ Alerting
                            AlertingRule
                              { alertingRuleName = AlertName "Site Down",
                                alertingRuleExpr =
                                  Binary
                                    isEqualTo
                                    OnEverything
                                    ( Vector
                                        (metric (MetricName "probe_success"))
                                          { selectorMatchers = [LabelName "env" `is` "production"]
                                          }
                                    )
                                    (Number 0),
                                alertingRuleFor = Just (Minutes 2),
                                alertingRuleKeepFiringFor = Just (Minutes 5),
                                alertingRuleLabels = Map.singleton (LabelName "severity") "page",
                                alertingRuleAnnotations =
                                  Map.fromList
                                    [ (LabelName "summary", "A public URL is not answering"),
                                      (LabelName "description", "{{ $labels.instance }} has been failing for over 2 minutes")
                                    ]
                              },
                          Recording
                            RecordingRule
                              { recordingRuleName = MetricName "env:probe_success:min",
                                recordingRuleExpr =
                                  Aggregate
                                    Minimum
                                    (Just (By (pure (LabelName "env"))))
                                    (Vector (metric (MetricName "probe_success"))),
                                recordingRuleLabels = Map.singleton (LabelName "aggregation") "min"
                              }
                        ]
                    },
                  RuleGroup
                    { ruleGroupName = RuleGroupName "empty",
                      ruleGroupInterval = Nothing,
                      ruleGroupRules = []
                    }
                ]
            }

    -- A long expression is written across lines, and has to come out of the
    -- file as the same text.
    it "writes an expression that is broken across lines" $
      let wide = Vector (metric (MetricName (mconcat (replicate 60 "a"))))
       in pureGoldenByteStringFile "test_resources/rule-file-long-expression.json" $
            renderRuleFile
              RuleFile
                { ruleFileGroups =
                    [ RuleGroup
                        { ruleGroupName = RuleGroupName "long",
                          ruleGroupInterval = Nothing,
                          ruleGroupRules =
                            [ Recording
                                RecordingRule
                                  { recordingRuleName = MetricName "long",
                                    recordingRuleExpr = Binary Divide OnEverything wide wide,
                                    recordingRuleLabels = Map.empty
                                  }
                            ]
                        }
                    ]
                }
