{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PromQL.RenderSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import PromQL
import qualified PromQL.Expr as Expr
import PromQL.Gen ()
import Test.QuickCheck (forAll, suchThat)
import Test.Syd
import Test.Syd.Validity
import Text.Read (readMaybe)

spec :: Spec
spec = do
  describe "Expr" $
    genValidSpec @Expr
  describe "Aggregation" $
    genValidSpec @Aggregation
  describe "Grouping" $
    genValidSpec @Grouping
  describe "BinOp" $
    genValidSpec @BinOp
  describe "Comparison" $
    genValidSpec @Comparison
  describe "Matching" $
    genValidSpec @Matching
  describe "Call" $
    genValidSpec @Call
  describe "LabelName" $
    genValidSpec @LabelName
  describe "MetricName" $
    genValidSpec @MetricName
  describe "RangeFunction" $
    genValidSpec @RangeFunction
  describe "RangeVector" $
    genValidSpec @RangeVector
  describe "Replacement" $
    genValidSpec @Replacement
  describe "Selector" $
    genValidSpec @Expr.Selector
  describe "Matcher" $
    genValidSpec @Matcher
  describe "Match" $
    genValidSpec @Match
  describe "Regexp" $
    genValidSpec @Regexp
  describe "Duration" $
    genValidSpec @Duration

  -- The whole reason an expression is a tree rather than text: PromQL accepts
  -- either grouping of the same operators and answers differently.
  describe "render" $ do
    let a = Vector (metric (MetricName "a"))
    let b = Vector (metric (MetricName "b"))
    let c = Vector (metric (MetricName "c"))
    let two = Number 2

    it "leaves out the brackets a reader of the grammar would put in anyway" $
      render (Binary Add OnEverything a (Binary Multiply OnEverything b c))
        `shouldBe` "a + b * c"

    it "brackets a sum that is divided" $
      render (Binary Divide OnEverything (Binary Add OnEverything a b) c)
        `shouldBe` "(a + b) / c"

    it "leaves the left-hand side of a subtraction alone" $
      render (Binary Subtract OnEverything (Binary Subtract OnEverything a b) c)
        `shouldBe` "a - b - c"

    -- Subtraction associates to the left, so the right-hand side of one is
    -- the side that needs the brackets.
    it "brackets the right-hand side of a subtraction that is itself one" $
      render (Binary Subtract OnEverything a (Binary Subtract OnEverything b c))
        `shouldBe` "a - (b - c)"

    -- Exponentiation is the one operator that associates to the right, so it
    -- is the other side that needs them.
    it "brackets the left-hand side of a power that is itself one" $
      render (Binary Power OnEverything (Binary Power OnEverything two two) two)
        `shouldBe` "(2 ^ 2) ^ 2"

    it "leaves the right-hand side of a power alone" $
      render (Binary Power OnEverything two (Binary Power OnEverything two two))
        `shouldBe` "2 ^ 2 ^ 2"

    it "brackets what a subquery samples" $
      render
        ( OverTime
            AverageOverTime
            (Sampled (Binary Add OnEverything a b) (Days 30) (Just (Hours 1)))
        )
        `shouldBe` "avg_over_time((a + b)[30d:1h])"

    -- An offset is written after the range it is taken over, and Prometheus
    -- refuses one written before.
    it "writes an offset after the range rather than before it" $
      render
        ( OverTime
            Rate
            (Over (metric (MetricName "a")) {selectorOffset = Just (Minutes 5)} (Hours 1))
        )
        `shouldBe` "rate(a[1h] offset 5m)"

    it "keeps a short expression on one line" $
      Text.lines (render (Binary Divide OnEverything a b))
        `shouldBe` ["a / b"]

    it "breaks a long expression at its loosest operator" $
      let long = Vector (metric (MetricName (Text.replicate 80 "a")))
       in Text.lines (render (Binary Divide OnEverything long long))
            `shouldSatisfy` ((> 1) . length)

  describe "renderMatcher" $ do
    let selectorWith matcher =
          render (Vector (metric (MetricName "x")) {selectorMatchers = [matcher]})

    it "writes a dot so that both the string and the regex read it as one" $
      selectorWith (LabelName "unit" `matching` Literally "a.service")
        `shouldBe` "x{unit=~\"a\\\\.service\"}"

    it "writes a backslash so that the regex reads it as one" $
      selectorWith (LabelName "unit" `matching` Literally "a\\b")
        `shouldBe` "x{unit=~\"a\\\\\\\\b\"}"

    it "offers each literal as an alternative" $
      selectorWith (LabelName "unit" `matching` AnyOf ("a.service" :| ["b.service"]))
        `shouldBe` "x{unit=~\"a\\\\.service|b\\\\.service\"}"

    it "matches a literal and anything after it" $
      selectorWith (LabelName "unit" `matching` StartingWith "worker")
        `shouldBe` "x{unit=~\"worker.*\"}"

    it "leaves a pattern as the pattern it was written as" $
      selectorWith (LabelName "code" `matching` Pattern "5..")
        `shouldBe` "x{code=~\"5..\"}"

    -- A lone backslash is what Prometheus rejects outright.
    it "writes backslashes in pairs, whatever the literal" $
      forAllValid $ \literal ->
        Text.replace "\\\\" "" (selectorWith (LabelName "unit" `matching` Literally literal))
          `shouldSatisfy` (not . Text.isInfixOf "\\")

  describe "renderNumber" $ do
    it "writes a whole number without a point" $
      renderNumber 2592 `shouldBe` "2592"

    -- Rounding a threshold that is a price is how one comes to mean
    -- something other than what it was given.
    it "keeps a monthly figure divided into a daily one" $
      renderNumber (500 / 30.4375) `shouldBe` "16.427104722792606"

    it "reads back as the number it was given" $
      forAll (genValid `suchThat` (isValid . Number)) $ \x ->
        readMaybe (Text.unpack (renderNumber x)) `shouldBe` Just x

  -- Every constructor's own text, because a renderer is a table and a table
  -- is only right one row at a time.  The end-to-end tests say Prometheus
  -- parses what we write; these say we wrote what we meant.
  describe "every case" $ do
    let a = Vector (metric (MetricName "a"))
    let b = Vector (metric (MetricName "b"))
    let two = Number 2
    let window = Over (metric (MetricName "a")) (Minutes 5)

    it "writes each function" $
      map
        (render . Apply)
        [ Absolute a,
          Ceiling a,
          Floor a,
          Exponential a,
          NaturalLog a,
          Log2 a,
          Log10 a,
          SquareRoot a,
          Sign a,
          Round a Nothing,
          Round a (Just two),
          Scalar a,
          ConstantVector two,
          ClampBelow a two,
          ClampAbove a two,
          Clamp a two two,
          Absent a,
          AbsentOverTime window,
          Timestamp a,
          HistogramQuantile two a,
          Sort a,
          SortDescending a
        ]
        `shouldBe` [ "abs(a)",
                     "ceil(a)",
                     "floor(a)",
                     "exp(a)",
                     "ln(a)",
                     "log2(a)",
                     "log10(a)",
                     "sqrt(a)",
                     "sgn(a)",
                     "round(a)",
                     "round(a, 2)",
                     "scalar(a)",
                     "vector(2)",
                     "clamp_min(a, 2)",
                     "clamp_max(a, 2)",
                     "clamp(a, 2, 2)",
                     "absent(a)",
                     "absent_over_time(a[5m])",
                     "timestamp(a)",
                     "histogram_quantile(2, a)",
                     "sort(a)",
                     "sort_desc(a)"
                   ]

    it "writes each range function" $
      map
        (\function -> render (OverTime function window))
        [minBound .. maxBound]
        `shouldBe` [ "rate(a[5m])",
                     "irate(a[5m])",
                     "increase(a[5m])",
                     "delta(a[5m])",
                     "idelta(a[5m])",
                     "deriv(a[5m])",
                     "changes(a[5m])",
                     "resets(a[5m])",
                     "avg_over_time(a[5m])",
                     "min_over_time(a[5m])",
                     "max_over_time(a[5m])",
                     "sum_over_time(a[5m])",
                     "count_over_time(a[5m])",
                     "last_over_time(a[5m])",
                     "present_over_time(a[5m])",
                     "stddev_over_time(a[5m])",
                     "stdvar_over_time(a[5m])"
                   ]

    it "writes the quantile over a window" $
      render (QuantileOverTime two window) `shouldBe` "quantile_over_time(2, a[5m])"

    it "writes each aggregation" $
      map
        (\aggregation -> render (Aggregate aggregation Nothing a))
        [ Sum,
          Average,
          Minimum,
          Maximum,
          Count,
          CountValues (LabelName "v"),
          Group,
          StandardDeviation,
          StandardVariance,
          TopK two,
          BottomK two,
          Quantile two
        ]
        `shouldBe` [ "sum (a)",
                     "avg (a)",
                     "min (a)",
                     "max (a)",
                     "count (a)",
                     "count_values (\"v\", a)",
                     "group (a)",
                     "stddev (a)",
                     "stdvar (a)",
                     "topk (2, a)",
                     "bottomk (2, a)",
                     "quantile (2, a)"
                   ]

    it "writes each way of grouping" $
      map
        (\grouping -> render (Aggregate Sum (Just grouping) a))
        [ By (LabelName "x" :| []),
          By (LabelName "x" :| [LabelName "y"]),
          Without (LabelName "x" :| [])
        ]
        `shouldBe` [ "sum by (x) (a)",
                     "sum by (x, y) (a)",
                     "sum without (x) (a)"
                   ]

    it "writes each operator" $
      map
        (\operator -> render (Binary operator OnEverything a b))
        [ Add,
          Subtract,
          Multiply,
          Divide,
          Modulo,
          Power,
          Atan2,
          Compare Equal False,
          Compare NotEqual False,
          Compare LessThan False,
          Compare LessOrEqual False,
          Compare GreaterThan False,
          Compare GreaterOrEqual False,
          Compare GreaterThan True,
          Or,
          And,
          Unless
        ]
        `shouldBe` [ "a + b",
                     "a - b",
                     "a * b",
                     "a / b",
                     "a % b",
                     "a ^ b",
                     "a atan2 b",
                     "a == b",
                     "a != b",
                     "a < b",
                     "a <= b",
                     "a > b",
                     "a >= b",
                     "a > bool b",
                     "a or b",
                     "a and b",
                     "a unless b"
                   ]

    it "writes each way of matching two sides up" $
      map
        (\matchOn -> render (Binary Divide matchOn a b))
        [ OnEverything,
          On [LabelName "x"],
          On [LabelName "x", LabelName "y"],
          Ignoring [LabelName "x"]
        ]
        `shouldBe` [ "a / b",
                     "a / on (x) b",
                     "a / on (x, y) b",
                     "a / ignoring (x) b"
                   ]

    it "writes each unit of time" $
      map
        (\duration -> render (OverTime Rate (Over (metric (MetricName "a")) duration)))
        [ Milliseconds 1,
          Seconds 2,
          Minutes 3,
          Hours 4,
          Days 5,
          Weeks 6,
          Years 7,
          DurationVariable "$__rate_interval"
        ]
        `shouldBe` [ "rate(a[1ms])",
                     "rate(a[2s])",
                     "rate(a[3m])",
                     "rate(a[4h])",
                     "rate(a[5d])",
                     "rate(a[6w])",
                     "rate(a[7y])",
                     "rate(a[$__rate_interval])"
                   ]

    it "writes each kind of matcher" $
      map
        (\match -> render (Vector (metric (MetricName "a")) {selectorMatchers = [Matcher (LabelName "l") match]}))
        [ Is "v",
          IsNot "v",
          Matches (Pattern "v.*"),
          DoesNotMatch (Pattern "v.*")
        ]
        `shouldBe` [ "a{l=\"v\"}",
                     "a{l!=\"v\"}",
                     "a{l=~\"v.*\"}",
                     "a{l!~\"v.*\"}"
                   ]

    it "writes a selector with no metric name" $
      render (Vector (series (LabelName "l" `is` "v" :| []))) `shouldBe` "{l=\"v\"}"

    it "writes a subquery with and without a step" $
      map
        (\step -> render (OverTime AverageOverTime (Sampled a (Days 30) step)))
        [Just (Hours 1), Nothing]
        `shouldBe` ["avg_over_time(a[30d:1h])", "avg_over_time(a[30d:])"]

    it "writes a label written from another" $
      render
        ( LabelReplace
            a
            Replacement
              { replacementDestination = LabelName "host",
                replacementText = "$1",
                replacementSource = LabelName "instance",
                replacementRegexp = Pattern "(.*)"
              }
        )
        `shouldBe` "label_replace(a, \"host\", \"$1\", \"instance\", \"(.*)\")"

    it "writes a window where a number is wanted" $
      render (Binary Multiply OnEverything a (Window (Minutes 5))) `shouldBe` "a * 5m"

    it "writes a string" $
      render (String "hello") `shouldBe` "\"hello\""

    it "writes every bracket when asked to" $
      renderBracketed (Binary Add OnEverything a (Binary Multiply OnEverything a b))
        `shouldBe` "(a + (a * b))"
