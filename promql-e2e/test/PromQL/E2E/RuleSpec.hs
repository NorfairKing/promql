{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rule and rule-test files against promtool.
--
-- Whether Prometheus loads a rule file, and what promtool makes of a series
-- written in its notation, are both questions only promtool can answer.
module PromQL.E2E.RuleSpec (spec) where

import Control.Exception (try)
import qualified Data.Map as Map
import PromQL
import PromQL.Rule
import PromQL.Rule.Gen (genSegments)
import PromQL.Rule.Promtool
import PromQL.Rule.Sydtest
import PromQL.Rule.Test
import Test.QuickCheck (forAll)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "checkRules" $
    it "writes a rule file Prometheus loads, whatever the rules" $
      forAllValid $ \ruleFile ->
        rulesShouldLoad ruleFile

  -- The notation is where a series is most easily wrong by one step, so
  -- every step is asked about twice: once over a window too short to see
  -- anything but that step, and once as an instant, which looks back over
  -- the steps before it and is the one that tells a gap from a staleness
  -- marker.
  describe "renderSegments" $
    it "means to promtool what expandSegments says it means" $
      forAll genSegments $ \segments -> do
        let x = MetricName "x"
        let xSeries =
              Series
                { seriesMetric = Just x,
                  seriesLabels = Map.empty
                }
        let samples = expandSegments segments
        let steps = zip [0 :: Word ..] samples
        let answeringWith = \case
              SampleValue value ->
                [ ExpectedSample
                    { expectedSampleSeries = xSeries,
                      expectedSampleValue = value
                    }
                ]
              SampleMissing -> []
              SampleStale -> []
        let atThisStep =
              [ ExprTest
                  { exprTestExpr = OverTime LastOverTime (Over (metric x) (Milliseconds 1)),
                    exprTestEvalTime = Minutes step,
                    exprTestExpectedSamples = answeringWith reading
                  }
              | (step, reading) <- steps
              ]
        -- An instant looks back five minutes, not including the step that
        -- is exactly five minutes back, and takes the latest sample there,
        -- unless that is a staleness marker.
        let lookingBack =
              [ ExprTest
                  { exprTestExpr = Vector (metric x),
                    exprTestEvalTime = Minutes step,
                    exprTestExpectedSamples =
                      case [ reading
                           | (earlier, reading) <- reverse steps,
                             earlier <= step,
                             earlier + 5 > step,
                             reading /= SampleMissing
                           ] of
                        [] -> []
                        latest : _ -> answeringWith latest
                  }
              | (step, _) <- steps
              ]
        rulesShouldPass
          RuleFile {ruleFileGroups = []}
          RuleTestFile
            { ruleTestFileEvaluationInterval = Nothing,
              ruleTestFileTests =
                [ RuleTest
                    { ruleTestName = renderSegments segments,
                      ruleTestInterval = Just (Minutes 1),
                      ruleTestExternalLabels = Map.empty,
                      ruleTestInputSeries =
                        [ InputSeries
                            { inputSeriesSeries = xSeries,
                              inputSeriesValues = segments
                            }
                        ],
                      ruleTestAlertTests = [],
                      ruleTestExprTests = atThisStep ++ lookingBack
                    }
                ]
            }

  -- A series is written in a selector's notation, which is where a name or a
  -- label value that needs quoting or escaping would come out as some other
  -- series.
  describe "renderSeries" $
    it "writes a series that promtool reads as that series" $
      forAllValid $ \input -> do
        let series' = inputSeriesSeries input
        let selectingIt =
              Selector
                { selectorMetric = seriesMetric series',
                  selectorMatchers = map (uncurry is) (Map.toList (seriesLabels series')),
                  selectorOffset = Nothing
                }
        rulesShouldPass
          RuleFile {ruleFileGroups = []}
          RuleTestFile
            { ruleTestFileEvaluationInterval = Nothing,
              ruleTestFileTests =
                [ RuleTest
                    { ruleTestName = renderSeries series',
                      ruleTestInterval = Nothing,
                      ruleTestExternalLabels = Map.empty,
                      ruleTestInputSeries = [input {inputSeriesValues = [sample 1]}],
                      ruleTestAlertTests = [],
                      ruleTestExprTests =
                        [ ExprTest
                            { exprTestExpr = Vector selectingIt,
                              exprTestEvalTime = Minutes 0,
                              exprTestExpectedSamples =
                                [ ExpectedSample
                                    { expectedSampleSeries = series',
                                      expectedSampleValue = 1
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }

  describe "ruleTestInterval" $
    it "is the evaluation interval where a test leaves it out" $
      let x =
            Series
              { seriesMetric = Just (MetricName "x"),
                seriesLabels = Map.empty
              }
       in rulesShouldPass
            RuleFile {ruleFileGroups = []}
            RuleTestFile
              { ruleTestFileEvaluationInterval = Just (Seconds 30),
                ruleTestFileTests =
                  [ RuleTest
                      { ruleTestName = "steps half a minute apart",
                        ruleTestInterval = Nothing,
                        ruleTestExternalLabels = Map.empty,
                        ruleTestInputSeries =
                          [ InputSeries
                              { inputSeriesSeries = x,
                                inputSeriesValues = [sample 0, sample 1]
                              }
                          ],
                        ruleTestAlertTests = [],
                        ruleTestExprTests =
                          [ ExprTest
                              { exprTestExpr = Vector (metric (MetricName "x")),
                                exprTestEvalTime = Seconds 30,
                                exprTestExpectedSamples =
                                  [ ExpectedSample
                                      { expectedSampleSeries = x,
                                        expectedSampleValue = 1
                                      }
                                  ]
                              }
                          ]
                      }
                  ]
              }

  -- The worked example: an alert that waits five minutes before it fires,
  -- tested on either side of those five minutes.
  describe "testRules" $ do
    let down =
          AlertingRule
            { alertingRuleName = AlertName "Instance Down",
              alertingRuleExpr = Binary isEqualTo OnEverything (Vector (metric (MetricName "up"))) (Number 0),
              alertingRuleFor = Just (Minutes 5),
              alertingRuleKeepFiringFor = Nothing,
              alertingRuleLabels = Map.singleton (LabelName "severity") "page",
              alertingRuleAnnotations =
                Map.singleton (LabelName "summary") "{{ $labels.instance }} is down"
            }
    let upByJob =
          RecordingRule
            { recordingRuleName = MetricName "job:up:sum",
              recordingRuleExpr =
                Aggregate Sum (Just (By (pure (LabelName "job")))) (Vector (metric (MetricName "up"))),
              recordingRuleLabels = Map.empty
            }
    let rules =
          RuleFile
            { ruleFileGroups =
                [ RuleGroup
                    { ruleGroupName = RuleGroupName "instances",
                      ruleGroupInterval = Nothing,
                      ruleGroupRules = [Alerting down, Recording upByJob]
                    }
                ]
            }
    let instance' = Map.fromList [(LabelName "job", "web"), (LabelName "instance", "a")]
    let firing =
          ExpectedAlert
            { expectedAlertLabels = Map.insert (LabelName "severity") "page" instance',
              expectedAlertAnnotations = Map.singleton (LabelName "summary") "a is down"
            }
    -- Up for five minutes and then down for ten, so that the alert is
    -- pending from five minutes in and firing from ten.
    let scenario alertTests =
          RuleTestFile
            { ruleTestFileEvaluationInterval = Nothing,
              ruleTestFileTests =
                [ RuleTest
                    { ruleTestName = "an instance goes down",
                      ruleTestInterval = Just (Minutes 1),
                      ruleTestExternalLabels = Map.empty,
                      ruleTestInputSeries =
                        [ InputSeries
                            { inputSeriesSeries =
                                Series
                                  { seriesMetric = Just (MetricName "up"),
                                    seriesLabels = instance'
                                  },
                              inputSeriesValues = [constant 1 5, constant 0 10]
                            }
                        ],
                      ruleTestAlertTests = alertTests,
                      ruleTestExprTests =
                        [ ExprTest
                            { exprTestExpr = Vector (metric (MetricName "job:up:sum")),
                              exprTestEvalTime = Minutes 2,
                              exprTestExpectedSamples =
                                [ ExpectedSample
                                    { expectedSampleSeries =
                                        Series
                                          { seriesMetric = Just (MetricName "job:up:sum"),
                                            seriesLabels = Map.singleton (LabelName "job") "web"
                                          },
                                      expectedSampleValue = 1
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }

    it "passes a test of when an alert fires" $
      rulesShouldPass
        rules
        ( scenario
            [ alertTest down (Minutes 4) [],
              alertTest down (Minutes 8) [],
              alertTest down (Minutes 10) [firing]
            ]
        )

    -- Pending is not firing, so an alert expected three minutes after the
    -- instance went down is one the rules do not send.
    it "fails a test that expects an alert before it fires, and says why" $
      goldenStringFile "test_resources/alert-test-failure.txt" $ do
        result <- testRules rules (scenario [alertTest down (Minutes 8) [firing]])
        case result of
          RuleTestsFailed output -> pure output
          RuleTestsPassed -> expectationFailure "promtool passed a test that expects an alert before it fires."
          AlertsWithoutRules names -> expectationFailure (unwords ["No rule defines", show names])

    -- promtool prints the alert's name, which is text in whatever script
    -- it was written in, and has to come back as that text even where the
    -- locale is not UTF-8.
    it "says why a test failed, in an alert whose name is not ASCII" $
      let café = down {alertingRuleName = AlertName "Café Down"}
          cafés =
            RuleFile
              { ruleFileGroups =
                  [ RuleGroup
                      { ruleGroupName = RuleGroupName "cafés",
                        ruleGroupInterval = Nothing,
                        ruleGroupRules = [Alerting café, Recording upByJob]
                      }
                  ]
              }
       in goldenStringFile "test_resources/alert-test-failure-non-ascii.txt" $ do
            result <- testRules cafés (scenario [alertTest café (Minutes 8) [firing]])
            case result of
              RuleTestsFailed output -> pure output
              RuleTestsPassed -> expectationFailure "promtool passed a test that expects an alert before it fires."
              AlertsWithoutRules names -> expectationFailure (unwords ["No rule defines", show names])

    -- promtool would pass this test, because it expects nothing to fire,
    -- whatever the rules said.
    it "refuses to test an alert that no rule defines" $ do
      result <- testRules rules (scenario [alertTest down {alertingRuleName = AlertName "Instance Gone"} (Minutes 4) []])
      result `shouldBe` AlertsWithoutRules (pure (AlertName "Instance Gone"))

    let brokenTemplate =
          RuleFile
            { ruleFileGroups =
                [ RuleGroup
                    { ruleGroupName = RuleGroupName "templates",
                      ruleGroupInterval = Nothing,
                      ruleGroupRules =
                        [ Alerting
                            down
                              { alertingRuleAnnotations =
                                  Map.singleton (LabelName "summary") "{{ $labels.instance"
                              }
                        ]
                    }
                ]
            }

    it "fails a rule file that Prometheus would not load, and says why" $
      goldenStringFile "test_resources/check-rules-failure.txt" $ do
        result <- checkRules brokenTemplate
        case result of
          PromtoolFailed output -> pure output
          PromtoolPassed -> expectationFailure "promtool loaded a rule whose template does not parse."

    -- What the assertions fail with is what promtool printed, as it printed
    -- it, which is the output that the golden files of the failures pin.
    describe "rulesShouldPass" $ do
      it "fails with what promtool printed" $ do
        output <- readFile "test_resources/alert-test-failure.txt"
        attempt <- try (rulesShouldPass rules (scenario [alertTest down (Minutes 8) [firing]]))
        attempt `shouldBe` Left (ExpectationFailed output)

      it "fails with the names of the alerts that no rule defines" $ do
        attempt <-
          try $
            rulesShouldPass
              rules
              ( scenario
                  [ alertTest down {alertingRuleName = AlertName "Instance Gone"} (Minutes 4) [],
                    alertTest down {alertingRuleName = AlertName "Instance Away"} (Minutes 4) []
                  ]
              )
        attempt
          `shouldBe` Left
            ( ExpectationFailed $
                unlines
                  [ "These alerts are tested, but no rule in the file defines them, so a test of them would pass whatever the rules said:",
                    "Instance Away",
                    "Instance Gone"
                  ]
            )

    describe "rulesShouldLoad" $
      it "fails with what promtool printed" $ do
        output <- readFile "test_resources/check-rules-failure.txt"
        attempt <- try (rulesShouldLoad brokenTemplate)
        attempt `shouldBe` Left (ExpectationFailed output)
