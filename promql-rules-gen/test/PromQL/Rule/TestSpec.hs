{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PromQL.Rule.TestSpec (spec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Either (isLeft)
import qualified Data.Map as Map
import Path (SomeBase (..), relfile)
import PromQL
import PromQL.Rule
import PromQL.Rule.Gen ()
import PromQL.Rule.Test
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  -- Each of these holds an expression or a ramp, and decodes as a value that
  -- encodes the same way: a ramp of one sample is written as its number,
  -- whatever it would have added, and some expressions are written alike.
  describe "RuleTestFile" $ do
    genValidSpec @RuleTestFile
    it "refuses a file that is not valid" $
      isLeft (JSON.parseEither (parseJSONViaCodec @RuleTestFile) (JSON.object [("evaluation_interval", JSON.String "$__interval")]))
        `shouldBe` True
    it "decodes what it encodes as a file that encodes the same way" $
      forAllValid $ \(value :: RuleTestFile) ->
        fmap (toJSONViaCodec @RuleTestFile) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "RuleTest" $ do
    genValidSpec @RuleTest
    it "decodes what it encodes as a test that encodes the same way" $
      forAllValid $ \(value :: RuleTest) ->
        fmap (toJSONViaCodec @RuleTest) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "InputSeries" $ do
    genValidSpec @InputSeries
    it "decodes what it encodes as a series that encodes the same way" $
      forAllValid $ \(value :: InputSeries) ->
        fmap (toJSONViaCodec @InputSeries) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "Series" $ do
    genValidSpec @Series
    it "decodes what it encodes" $
      forAllValid $ \(value :: Series) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value
  describe "Segment" $ do
    genValidSpec @Segment
    it "refuses more than one segment" $
      JSON.parseEither (parseJSONViaCodec @Segment) (JSON.String "1 2")
        `shouldBe` Left "Error in $: one segment, not 2"
    it "decodes what it encodes as a segment that encodes the same way" $
      forAllValid $ \(value :: Segment) ->
        fmap (toJSONViaCodec @Segment) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "Ramp" $ do
    genValidSpec @Ramp
    it "decodes what it encodes as a ramp that encodes the same way" $
      forAllValid $ \(value :: Ramp) ->
        fmap (toJSONViaCodec @Ramp) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "AlertTest" $ do
    genValidSpec @AlertTest
    it "decodes what it encodes" $
      forAllValid $ \(value :: AlertTest) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value
  describe "ExpectedAlert" $ do
    genValidSpec @ExpectedAlert
    it "decodes what it encodes" $
      forAllValid $ \(value :: ExpectedAlert) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value
  describe "ExprTest" $ do
    genValidSpec @ExprTest
    it "decodes what it encodes as a test that encodes the same way" $
      forAllValid $ \(value :: ExprTest) ->
        fmap (toJSONViaCodec @ExprTest) (JSON.parseEither parseJSONViaCodec (toJSONViaCodec value))
          `shouldBe` Right (toJSONViaCodec value)
  describe "ExpectedSample" $ do
    genValidSpec @ExpectedSample
    it "decodes what it encodes" $
      forAllValid $ \(value :: ExpectedSample) ->
        JSON.parseEither parseJSONViaCodec (toJSONViaCodec value) `shouldBe` Right value

  describe "parseSeries" $ do
    it "reads back what renderSeries writes" $
      forAllValid $ \series' ->
        parseSeries (renderSeries series') `shouldBe` Right series'
    it "refuses a selector that is not one series, and says why" $
      map parseSeries ["up{job!=\"a\"}", "up{job=~\"a\"}", "up{job!~\"a\"}", "up{job=\"a\", job=\"b\"}", "up offset 5m"]
        `shouldBe` map
          Left
          [ SeriesNotExact (LabelName "job"),
            SeriesNotExact (LabelName "job"),
            SeriesNotExact (LabelName "job"),
            SeriesLabelRepeated (LabelName "job"),
            SeriesOffset
          ]
    it "refuses what is not a selector at all" $
      case parseSeries "up{" of
        Left (SeriesNotASelector _) -> pure ()
        other -> expectationFailure (show other)

  describe "renderSeriesError" $
    it "says which label a series gave the wrong value" $
      map
        renderSeriesError
        [SeriesOffset, SeriesNotExact (LabelName "job"), SeriesLabelRepeated (LabelName "job")]
        `shouldBe` [ "a series has no offset",
                     "a series has one value for \"job\"",
                     "a series has one value for \"job\" and this gives it more"
                   ]

  describe "parseSegments" $ do
    it "reads back what renderSegments writes as the same steps" $
      forAllValid $ \segments ->
        fmap expandSegments (parseSegments (renderSegments segments)) `shouldBe` Right (expandSegments segments)
    it "reads back what renderSegments writes as segments written the same way" $
      forAllValid $ \segments ->
        fmap renderSegments (parseSegments (renderSegments segments)) `shouldBe` Right (renderSegments segments)
    -- promtool reads 0x3 as zero and three more steps of it, where a query
    -- would read a hexadecimal three.
    it "reads each kind of segment as promtool does" $
      parseSegments "0x3 1+2x3 -5-0.5x2 1.0e-2 _ _x3 stale"
        `shouldBe` Right
          [ constant 0 4,
            Samples
              Ramp
                { rampStart = 1,
                  rampIncrement = 2,
                  rampCount = 4
                },
            Samples
              Ramp
                { rampStart = -5,
                  rampIncrement = -0.5,
                  rampCount = 3
                },
            sample 0.01,
            Missing 1,
            Missing 3,
            Stale
          ]
    it "refuses what is not a segment" $
      map (isLeft . parseSegments) ["x3", "1x", "1+x3", "_x", "fresh"]
        `shouldBe` [True, True, True, True, True]
    it "reads the largest counts there are" $
      parseSegments "1x18446744073709551614 _x18446744073709551615"
        `shouldBe` Right [constant 1 maxBound, Missing maxBound]
    -- Rather than as a count that wrapped around to a small one.
    it "refuses a count larger than there are steps to count" $
      map (isLeft . parseSegments) ["1x18446744073709551615", "1+1x18446744073709551615", "_x18446744073709551616"]
        `shouldBe` [True, True, True]

  describe "validity" $ do
    let ramp count =
          Ramp
            { rampStart = 1,
              rampIncrement = 1,
              rampCount = count
            }

    it "refuses a segment that covers no steps" $
      map isValid [Samples (ramp 0), Missing 0, Samples (ramp 1), Missing 1, Stale]
        `shouldBe` [False, False, True, True, True]

    it "refuses a ramp of numbers a test file cannot carry" $
      map
        (\x -> (isValid (ramp 1) {rampStart = x}, isValid (ramp 1) {rampIncrement = x}))
        [0 / 0, 1 / 0, -(1 / 0)]
        `shouldBe` replicate 3 (False, False)

    it "refuses a ramp that climbs past what a number can hold" $
      map
        ( \(start, increment) ->
            isValid
              Ramp
                { rampStart = start,
                  rampIncrement = increment,
                  rampCount = 3
                }
        )
        [(1e308, 1e308), (-1e308, -1e308), (1e308, -1e308)]
        `shouldBe` [False, False, True]

    it "refuses an expected value a test file cannot carry" $
      map
        ( \value ->
            isValid
              ExpectedSample
                { expectedSampleSeries =
                    Series
                      { seriesMetric = Just (MetricName "up"),
                        seriesLabels = Map.empty
                      },
                  expectedSampleValue = value
                }
        )
        [0 / 0, 1 / 0, 1]
        `shouldBe` [False, False, True]

    -- An empty label value is a label that is not there, so allowing one
    -- would give one series two spellings.
    it "refuses a series with an empty label value" $
      isValid
        Series
          { seriesMetric = Just (MetricName "up"),
            seriesLabels = Map.singleton (LabelName "job") ""
          }
        `shouldBe` False

    -- A series is written in a selector's notation, which Prometheus refuses
    -- with U+FFFD in it.
    it "refuses a series whose label value holds U+FFFD" $
      isValid
        Series
          { seriesMetric = Just (MetricName "up"),
            seriesLabels = Map.singleton (LabelName "job") "a\xFFFD"
          }
        `shouldBe` False

    it "refuses a series that names its metric among its labels" $
      isValid
        Series
          { seriesMetric = Nothing,
            seriesLabels = Map.singleton (LabelName "__name__") "up"
          }
        `shouldBe` False

    -- promtool refuses one, with "empty labelset: invalid sample".
    it "refuses an input series with neither a name nor labels" $
      map
        ( \(name, labels) ->
            isValid
              InputSeries
                { inputSeriesSeries =
                    Series
                      { seriesMetric = name,
                        seriesLabels = labels
                      },
                  inputSeriesValues = [sample 1]
                }
        )
        [ (Nothing, Map.empty),
          (Nothing, Map.singleton (LabelName "job") "a"),
          (Just (MetricName "up"), Map.empty)
        ]
        `shouldBe` [False, True, True]

    -- promtool keeps only the last of the two, without saying so.
    it "refuses a test that gives the same series twice" $
      let input name value =
            InputSeries
              { inputSeriesSeries =
                  Series
                    { seriesMetric = Just (MetricName name),
                      seriesLabels = Map.empty
                    },
                inputSeriesValues = [sample value]
              }
          test inputs =
            RuleTest
              { ruleTestName = "t",
                ruleTestInterval = Nothing,
                ruleTestExternalLabels = Map.empty,
                ruleTestInputSeries = inputs,
                ruleTestAlertTests = [],
                ruleTestExprTests = []
              }
       in map
            isValid
            [ test [input "up" 1, input "up" 5],
              test [input "up" 1, input "down" 5]
            ]
            `shouldBe` [False, True]

    -- A series is answered with at most once, and an alert with the same
    -- labels fires at most once, so a test expecting either twice could
    -- never pass.
    it "refuses a test that expects the same series or the same alert twice" $
      let up =
            Series
              { seriesMetric = Just (MetricName "up"),
                seriesLabels = Map.empty
              }
          exprTest expected =
            ExprTest
              { exprTestExpr = Vector (metric (MetricName "up")),
                exprTestEvalTime = Minutes 1,
                exprTestExpectedSamples = expected
              }
          expectedSample value =
            ExpectedSample
              { expectedSampleSeries = up,
                expectedSampleValue = value
              }
          alertTest' expected =
            AlertTest
              { alertTestEvalTime = Minutes 1,
                alertTestAlertName = AlertName "Down",
                alertTestExpectedAlerts = expected
              }
          firing =
            ExpectedAlert
              { expectedAlertLabels = Map.singleton (LabelName "job") "a",
                expectedAlertAnnotations = Map.empty
              }
          annotated = firing {expectedAlertAnnotations = Map.singleton (LabelName "a") "b"}
       in [ isValid (exprTest [expectedSample 1, expectedSample 2]),
            isValid (exprTest [expectedSample 1]),
            isValid (alertTest' [firing, annotated]),
            isValid (alertTest' [firing])
          ]
            `shouldBe` [False, True, False, True]

    -- Each of these holds something that is not valid one level down, so
    -- that validity is seen to reach into what each type holds.
    it "refuses a test file, a test or an expectation holding something that is not valid" $
      let variable = DurationVariable "$__interval"
          notAName =
            Series
              { seriesMetric = Just (MetricName "0a"),
                seriesLabels = Map.empty
              }
          testFile interval tests =
            RuleTestFile
              { ruleTestFileEvaluationInterval = interval,
                ruleTestFileTests = tests
              }
          test =
            RuleTest
              { ruleTestName = "t",
                ruleTestInterval = Nothing,
                ruleTestExternalLabels = Map.empty,
                ruleTestInputSeries = [],
                ruleTestAlertTests = [],
                ruleTestExprTests = []
              }
          exprTest =
            ExprTest
              { exprTestExpr = Vector (metric (MetricName "up")),
                exprTestEvalTime = Minutes 1,
                exprTestExpectedSamples = []
              }
          alertTest' name =
            AlertTest
              { alertTestEvalTime = Minutes 1,
                alertTestAlertName = AlertName name,
                alertTestExpectedAlerts = []
              }
       in [ isValid (testFile Nothing [test]),
            isValid (testFile (Just variable) [test]),
            isValid (testFile Nothing [test {ruleTestInterval = Just variable}]),
            isValid
              test
                { ruleTestInputSeries =
                    [ InputSeries
                        { inputSeriesSeries = notAName,
                          inputSeriesValues = []
                        }
                    ]
                },
            isValid exprTest,
            isValid exprTest {exprTestEvalTime = variable},
            isValid exprTest {exprTestExpr = Number (0 / 0)},
            isValid
              ExpectedSample
                { expectedSampleSeries = notAName,
                  expectedSampleValue = 1
                },
            isValid notAName,
            isValid (alertTest' "Down"),
            isValid (alertTest' "")
          ]
            `shouldBe` [True, False, False, False, True, False, False, False, False, True, False]

    it "refuses a time to look that is a variable" $
      map
        ( \evalTime ->
            isValid
              AlertTest
                { alertTestEvalTime = evalTime,
                  alertTestAlertName = AlertName "Down",
                  alertTestExpectedAlerts = []
                }
        )
        [DurationVariable "$__interval", Minutes 5]
        `shouldBe` [False, True]

  -- promtool's notation counts a run of samples from zero and a run of gaps
  -- from one, which is exactly the kind of thing that is wrong by one.
  describe "renderSegments" $ do
    let ramp start increment count =
          Samples
            Ramp
              { rampStart = start,
                rampIncrement = increment,
                rampCount = count
              }

    it "writes a single sample as its number" $
      renderSegments [sample 1, sample 1.5, sample (-2)] `shouldBe` "1 1.5 -2"

    it "writes a constant as the steps after its first" $
      renderSegments [constant 0 10] `shouldBe` "0x9"

    it "writes a ramp up and a ramp down" $
      renderSegments
        [ ramp 1 2 4,
          ramp (-5) (-0.5) 3,
          ramp 0 0.5 2
        ]
        `shouldBe` "1+2x3 -5-0.5x2 0+0.5x1"

    it "writes the gaps as every one of them" $
      renderSegments [Missing 1, Missing 3] `shouldBe` "_ _x3"

    it "writes a staleness marker" $
      renderSegments [sample 1, Stale] `shouldBe` "1 stale"

    it "writes no segments as nothing" $
      renderSegments [] `shouldBe` ""

  describe "expandSegments" $ do
    let ramp start increment count =
          Samples
            Ramp
              { rampStart = start,
                rampIncrement = increment,
                rampCount = count
              }

    it "means as many steps as each segment says it covers" $
      expandSegments [constant 0 3, Missing 2, Stale, ramp 1 2 3]
        `shouldBe` [ SampleValue 0,
                     SampleValue 0,
                     SampleValue 0,
                     SampleMissing,
                     SampleMissing,
                     SampleStale,
                     SampleValue 1,
                     SampleValue 3,
                     SampleValue 5
                   ]

    -- promtool adds the increment at each step, and multiplying it out
    -- would disagree in the last digits.
    it "sums a fractional ramp one step at a time" $
      expandSegments [ramp 0.1 0.1 3]
        `shouldBe` [SampleValue 0.1, SampleValue 0.2, SampleValue 0.30000000000000004]

    it "means as many steps as the segments cover, whatever they are" $
      forAllValid $ \segments ->
        length (expandSegments segments)
          `shouldBe` sum
            ( map
                ( \case
                    Samples ramp' -> fromEnum (rampCount ramp')
                    Missing count -> fromEnum count
                    Stale -> 1
                )
                segments
            )

  describe "renderSeries" $
    it "writes a series with and without a name and labels" $
      map
        ( \(name, labels) ->
            renderSeries
              Series
                { seriesMetric = name,
                  seriesLabels = Map.fromList labels
                }
        )
        [ (Just (MetricName "up"), []),
          (Just (MetricName "up"), [(LabelName "job", "a"), (LabelName "instance", "b\"c")]),
          (Nothing, [(LabelName "job", "a")]),
          (Nothing, [])
        ]
        `shouldBe` ["up", "up{instance=\"b\\\"c\", job=\"a\"}", "{job=\"a\"}", "{}"]

  describe "alertTest" $
    it "tests the alert by the name of the rule it is given" $
      let firing =
            ExpectedAlert
              { expectedAlertLabels = Map.singleton (LabelName "job") "web",
                expectedAlertAnnotations = Map.empty
              }
       in alertTest
            AlertingRule
              { alertingRuleName = AlertName "Down",
                alertingRuleExpr = Vector (metric (MetricName "up")),
                alertingRuleFor = Nothing,
                alertingRuleKeepFiringFor = Nothing,
                alertingRuleLabels = Map.empty,
                alertingRuleAnnotations = Map.empty
              }
            (Minutes 5)
            [firing]
            `shouldBe` AlertTest
              { alertTestEvalTime = Minutes 5,
                alertTestAlertName = AlertName "Down",
                alertTestExpectedAlerts = [firing]
              }

  -- promtool passes a test of an alert that no rule defines, as long as it
  -- expects nothing to fire, so such a test would pass whatever the rules.
  describe "alertsWithoutRules" $
    it "names every alert that is tested but that no rule defines, once" $
      let alerting name =
            Alerting
              AlertingRule
                { alertingRuleName = AlertName name,
                  alertingRuleExpr = Vector (metric (MetricName "up")),
                  alertingRuleFor = Nothing,
                  alertingRuleKeepFiringFor = Nothing,
                  alertingRuleLabels = Map.empty,
                  alertingRuleAnnotations = Map.empty
                }
          recording =
            Recording
              RecordingRule
                { recordingRuleName = MetricName "Recorded",
                  recordingRuleExpr = Vector (metric (MetricName "up")),
                  recordingRuleLabels = Map.empty
                }
          group name rules =
            RuleGroup
              { ruleGroupName = RuleGroupName name,
                ruleGroupInterval = Nothing,
                ruleGroupRules = rules
              }
          testing names =
            RuleTest
              { ruleTestName = "t",
                ruleTestInterval = Nothing,
                ruleTestExternalLabels = Map.empty,
                ruleTestInputSeries = [],
                ruleTestAlertTests =
                  [ AlertTest
                      { alertTestEvalTime = Minutes 1,
                        alertTestAlertName = AlertName name,
                        alertTestExpectedAlerts = []
                      }
                  | name <- names
                  ],
                ruleTestExprTests = []
              }
       in alertsWithoutRules
            RuleFile
              { ruleFileGroups =
                  [ group "a" [alerting "Down", recording],
                    group "b" [alerting "Slow"]
                  ]
              }
            RuleTestFile
              { ruleTestFileEvaluationInterval = Nothing,
                ruleTestFileTests =
                  [ testing ["Down", "Missing", "Recorded"],
                    testing ["Slow", "Missing"]
                  ]
              }
            `shouldBe` [AlertName "Missing", AlertName "Recorded"]

  describe "renderRuleTestFile" $
    it "writes a file with one test of each kind" $
      let up =
            Series
              { seriesMetric = Just (MetricName "up"),
                seriesLabels = Map.fromList [(LabelName "job", "web"), (LabelName "instance", "a")]
              }
       in pureGoldenByteStringFile "test_resources/rule-test-file.json" $
            renderRuleTestFile
              [Rel [relfile|rules.json|]]
              RuleTestFile
                { ruleTestFileEvaluationInterval = Just (Seconds 30),
                  ruleTestFileTests =
                    [ RuleTest
                        { ruleTestName = "the web server goes down",
                          ruleTestInterval = Just (Minutes 1),
                          ruleTestExternalLabels = Map.singleton (LabelName "cluster") "eu",
                          ruleTestInputSeries =
                            [ InputSeries
                                { inputSeriesSeries = up,
                                  inputSeriesValues = [constant 1 5, constant 0 10, Missing 2, Stale]
                                }
                            ],
                          ruleTestAlertTests =
                            [ AlertTest
                                { alertTestEvalTime = Minutes 4,
                                  alertTestAlertName = AlertName "Down",
                                  alertTestExpectedAlerts = []
                                },
                              AlertTest
                                { alertTestEvalTime = Minutes 12,
                                  alertTestAlertName = AlertName "Down",
                                  alertTestExpectedAlerts =
                                    [ ExpectedAlert
                                        { expectedAlertLabels =
                                            Map.fromList
                                              [ (LabelName "job", "web"),
                                                (LabelName "instance", "a"),
                                                (LabelName "severity", "page")
                                              ],
                                          expectedAlertAnnotations =
                                            Map.singleton (LabelName "summary") "a is down"
                                        }
                                    ]
                                }
                            ],
                          ruleTestExprTests =
                            [ ExprTest
                                { exprTestExpr = Aggregate Sum Nothing (Vector (metric (MetricName "up"))),
                                  exprTestEvalTime = Minutes 2,
                                  exprTestExpectedSamples =
                                    [ ExpectedSample
                                        { expectedSampleSeries =
                                            Series
                                              { seriesMetric = Nothing,
                                                seriesLabels = Map.empty
                                              },
                                          expectedSampleValue = 1
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
