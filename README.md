# promql

PromQL as a type rather than as text.

## Status

Not ready.

## Packages

* `promql`: an expression type for PromQL, and the renderer that writes it.
* `promql-gen`: generators for it.
* `promql-rules`: alerting and recording rules, the file Prometheus reads them
  from, and unit tests for them in the format `promtool test rules` reads.
* `promql-rules-gen`: generators for those, and sydtest assertions that run
  the tests with `promtool`.

## Testing alerts

A test says what the series held, one step after another, and what the
rules should have made of them at some moment.
promtool evaluates the rules with Prometheus's own engine, so it has to be on
the `PATH` of the test suite.

```haskell
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
```

This is `promql-e2e/test/PromQL/E2E/ReadmeSpec.hs`, with `OverloadedStrings`
on, and it runs as part of that suite.
An example with a recording rule as well, and of what a failure looks like, is
in `promql-e2e/test/PromQL/E2E/RuleSpec.hs`.
