{-# LANGUAGE OverloadedStrings #-}

-- | The example in the README, run.
--
-- Everything from the imports down is the README's example, and the
-- readme-example-check in the flake fails when the two differ, so an example
-- that no longer compiles or no longer passes cannot stay in the README.
module PromQL.E2E.ReadmeSpec (spec) where

import qualified Data.Map as Map
import PromQL
import PromQL.Rule
import PromQL.Rule.Sydtest
import PromQL.Rule.Test
import Test.Syd

spec :: Spec
spec =
  it "pages once an instance has been down for five minutes" $
    let down =
          AlertingRule
            { alertingRuleName = AlertName "Instance Down",
              alertingRuleExpr = Binary isEqualTo OnEverything (Vector (metric (MetricName "up"))) (Number 0),
              alertingRuleFor = Just (Minutes 5),
              alertingRuleKeepFiringFor = Nothing,
              alertingRuleLabels = Map.singleton (LabelName "severity") "page",
              alertingRuleAnnotations = Map.singleton (LabelName "summary") "{{ $labels.instance }} is down"
            }
        instance' = Map.fromList [(LabelName "job", "web"), (LabelName "instance", "a")]
     in rulesShouldPass
          RuleFile
            { ruleFileGroups =
                [ RuleGroup
                    { ruleGroupName = RuleGroupName "instances",
                      ruleGroupInterval = Nothing,
                      ruleGroupRules = [Alerting down]
                    }
                ]
            }
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
                              -- Up for five minutes, then down for ten.
                              inputSeriesValues = [constant 1 5, constant 0 10]
                            }
                        ],
                      ruleTestAlertTests =
                        [ -- Pending, which is not firing.
                          alertTest down (Minutes 8) [],
                          alertTest
                            down
                            (Minutes 10)
                            [ ExpectedAlert
                                { expectedAlertLabels = Map.insert (LabelName "severity") "page" instance',
                                  expectedAlertAnnotations = Map.singleton (LabelName "summary") "a is down"
                                }
                            ]
                        ],
                      ruleTestExprTests = []
                    }
                ]
            }
