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
  describe "Answering" $
    genValidSpec @Answering
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
          Compare Equal AsAFilter,
          Compare NotEqual AsAFilter,
          Compare LessThan AsAFilter,
          Compare LessOrEqual AsAFilter,
          Compare GreaterThan AsAFilter,
          Compare GreaterOrEqual AsAFilter,
          Compare GreaterThan AsZeroOrOne,
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
        (render . OverTime Rate . Over (metric (MetricName "a")))
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
        (render . OverTime AverageOverTime . Sampled a (Days 30))
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

  -- The layout decides where a query breaks and how far each piece is
  -- indented, which is arithmetic over columns and so is wrong one character
  -- at a time.  These pin the exact text, because nothing else can.
  describe "layOut" $ do
    let wide n = Vector (metric (MetricName (Text.replicate n "a")))
    let as n = Text.replicate n "a"

    -- queryWidth characters exactly, which is the last width that fits.
    it "keeps a query of exactly the width it is allowed on one line" $
      let rendered = render (Binary Divide OnEverything (wide 53) (wide 54))
       in (Text.length rendered, rendered)
            `shouldBe` (queryWidth, Text.concat [as 53, " / ", as 54])

    it "breaks a query one character wider than it is allowed" $
      render (Binary Divide OnEverything (wide 53) (wide 55))
        `shouldBe` Text.concat [as 53, "\n  / ", as 55]

    -- The right-hand side is laid out from the column its operator left it
    -- at, so this pins the operator's width and the spaces either side of it
    -- as well as the two-space indent.
    it "lays the right-hand side out from where the operator leaves it" $
      render
        ( Binary
            Divide
            OnEverything
            (wide 53)
            (Binary Add OnEverything (wide 60) (wide 60))
        )
        `shouldBe` Text.concat
          [ as 53,
            "\n  / (",
            as 60,
            "\n       + ",
            as 60,
            ")"
          ]

    -- How two sides are matched up is written beside the operator, so it
    -- widens the joint and moves what follows.
    it "counts the vector matching as part of the operator" $
      render (Binary Divide (On [LabelName "x"]) (wide 53) (wide 60))
        `shouldBe` Text.concat [as 53, "\n  / on (x) ", as 60]

    -- A call puts each argument on its own line, indented, with the closing
    -- bracket back at the call's own column.
    it "breaks a call between its arguments" $
      render (Apply (ClampBelow (wide 100) (Number 1)))
        `shouldBe` Text.concat ["clamp_min(\n  ", as 100, ",\n  1\n)"]

    it "leaves a call that fits alone" $
      render (Apply (ClampBelow (wide 10) (Number 1)))
        `shouldBe` Text.concat ["clamp_min(", as 10, ", 1)"]

    -- A selector is as long as its labels make it and has nowhere to break,
    -- so it is written out however wide it is.
    it "writes a selector too wide to fit rather than breaking it" $
      render (wide 200) `shouldBe` as 200

  -- The bounds and the spellings a type refuses, which are what stop a query
  -- carrying something Prometheus will not read.
  describe "validity" $ do
    it "takes each unit of time up to what Prometheus has room for" $
      map
        isValid
        [ Milliseconds 9223372036854,
          Seconds 9223372036,
          Minutes 153722867,
          Hours 2562047,
          Days 106751,
          Weeks 15250,
          Years 292
        ]
        `shouldBe` [True, True, True, True, True, True, True]

    it "refuses each unit of time one past that" $
      map
        isValid
        [ Milliseconds 9223372036855,
          Seconds 9223372037,
          Minutes 153722868,
          Hours 2562048,
          Days 106752,
          Weeks 15251,
          Years 293
        ]
        `shouldBe` [False, False, False, False, False, False, False]

    it "takes a window written as something to fill in later" $
      isValid (DurationVariable "$__rate_interval") `shouldBe` True

    it "takes a label name that is an identifier" $
      map (isValid . LabelName) ["a", "_", "A", "a_B9", "_0"]
        `shouldBe` [True, True, True, True, True]

    it "refuses a label name that is not one" $
      map (isValid . LabelName) ["", "0a", "a-b", "a:b", "a b", "a.b"]
        `shouldBe` [False, False, False, False, False, False]

    -- A metric name may carry colons, which are reserved for recording rules.
    it "takes a metric name with a colon in it" $
      map (isValid . MetricName) ["a", ":a", "a:b", "a:b:c"]
        `shouldBe` [True, True, True, True]

    it "refuses a metric name that is not one" $
      map (isValid . MetricName) ["", "0a", "a-b", "a b"]
        `shouldBe` [False, False, False, False]

    it "makes a name out of text that is one, and nothing out of text that is not" $
      (labelName "a", labelName "0a", metricName "a:b", metricName "0a")
        `shouldBe` (Just (LabelName "a"), Nothing, Just (MetricName "a:b"), Nothing)

    -- A selector with neither a metric nor a matcher would select every
    -- series there is.
    it "refuses a selector that says nothing about which series it means" $
      isValid
        Selector
          { selectorMetric = Nothing,
            selectorMatchers = [],
            selectorOffset = Nothing
          }
        `shouldBe` False

    it "refuses a number a query cannot carry" $
      map (isValid . Number) [0 / 0, 1 / 0, -(1 / 0)] `shouldBe` [False, False, False]

  -- Every shape again, this time too wide to fit, because what a query is
  -- written as when it breaks goes through a different road in the renderer
  -- from what it is written as when it does not.
  describe "every case, laid out across lines" $ do
    let as n = Text.replicate n "a"
    let w = Vector (metric (MetricName (as 120)))
    let window = Over (metric (MetricName (as 120))) (Minutes 5)

    it "breaks a range function between its brackets" $
      render (OverTime Rate window)
        `shouldBe` Text.concat ["rate(\n  ", as 120, "[5m]\n)"]

    it "breaks a quantile over a window between its arguments" $
      render (QuantileOverTime (Number 2) window)
        `shouldBe` Text.concat ["quantile_over_time(\n  2,\n  ", as 120, "[5m]\n)"]

    it "breaks a label written from another between its arguments" $
      render
        ( LabelReplace
            w
            Replacement
              { replacementDestination = LabelName "h",
                replacementText = "$1",
                replacementSource = LabelName "i",
                replacementRegexp = Pattern "(.*)"
              }
        )
        `shouldBe` Text.concat
          ["label_replace(\n  ", as 120, ",\n  \"h\",\n  \"$1\",\n  \"i\",\n  \"(.*)\"\n)"]

    it "keeps an aggregation's grouping beside its name when it breaks" $
      render (Aggregate Sum (Just (By (LabelName "x" :| [LabelName "y"]))) w)
        `shouldBe` Text.concat ["sum by (x, y) (\n  ", as 120, "\n)"]

    it "writes an aggregation with no grouping when it breaks" $
      render (Aggregate Sum Nothing w)
        `shouldBe` Text.concat ["sum (\n  ", as 120, "\n)"]

    -- An aggregation that takes a number takes it before the vector, on a
    -- line of its own once it breaks.
    it "breaks an aggregation between its number and its vector" $
      render (Aggregate (TopK (Number 3)) Nothing w)
        `shouldBe` Text.concat ["topk (\n  3,\n  ", as 120, "\n)"]

  -- The precedence table, at every step of it.  A mutation to any one number
  -- shows up as a bracket that appears or disappears.
  describe "precedence" $ do
    let a = Vector (metric (MetricName "a"))

    it "binds a remainder more tightly than a sum" $
      map
        render
        [ Binary Add OnEverything a (Binary Modulo OnEverything a a),
          Binary Modulo OnEverything (Binary Add OnEverything a a) a
        ]
        `shouldBe` ["a + a % a", "(a + a) % a"]

    it "binds atan2 more tightly than a sum" $
      map
        render
        [ Binary Add OnEverything a (Binary Atan2 OnEverything a a),
          Binary Atan2 OnEverything (Binary Add OnEverything a a) a
        ]
        `shouldBe` ["a + a atan2 a", "(a + a) atan2 a"]

    it "binds a comparison more loosely than a sum and more tightly than and" $
      map
        render
        [ Binary (Compare GreaterThan AsAFilter) OnEverything (Binary Add OnEverything a a) a,
          Binary And OnEverything (Binary (Compare GreaterThan AsAFilter) OnEverything a a) a
        ]
        `shouldBe` ["a + a > a", "a > a and a"]

    it "binds and more tightly than or" $
      map
        render
        [ Binary Or OnEverything (Binary And OnEverything a a) a,
          Binary And OnEverything (Binary Or OnEverything a a) a
        ]
        `shouldBe` ["a and a or a", "(a or a) and a"]

    it "binds unless as tightly as and" $
      render (Binary And OnEverything (Binary Unless OnEverything a a) a)
        `shouldBe` "a unless a and a"

    it "binds a power more tightly than a product" $
      map
        render
        [ Binary Multiply OnEverything a (Binary Power OnEverything a a),
          Binary Power OnEverything (Binary Multiply OnEverything a a) a
        ]
        `shouldBe` ["a * a ^ a", "(a * a) ^ a"]

    -- Nothing can come between the parts of a name, a number or something
    -- with its own brackets, so none of them is ever bracketed again.
    it "never brackets what already has brackets of its own" $
      render (Binary Power OnEverything (Apply (Absolute a)) (Aggregate Sum Nothing a))
        `shouldBe` "abs(a) ^ sum (a)"

  describe "selector validity" $
    it "takes a selector that names only labels" $
      isValid
        Selector
          { selectorMetric = Nothing,
            selectorMatchers = [LabelName "l" `is` "v"],
            selectorOffset = Nothing
          }
        `shouldBe` True

  describe "renderBracketed" $ do
    let a = Vector (metric (MetricName "a"))
    let b = Vector (metric (MetricName "b"))

    it "leaves a call's own brackets as the only ones it needs" $
      renderBracketed (Apply (ClampBelow a (Number 1)))
        `shouldBe` "clamp_min(a, 1)"

    it "keeps the vector matching beside the operator" $
      renderBracketed (Binary Divide (On [LabelName "x"]) a b)
        `shouldBe` "(a / on (x) b)"

    it "brackets a name on its own not at all" $
      renderBracketed a `shouldBe` "a"

  describe "renderSelector" $ do
    it "writes an offset after the metric it is taken on" $
      render (Vector (metric (MetricName "a")) {selectorOffset = Just (Minutes 5)})
        `shouldBe` "a offset 5m"

    it "writes a selector with labels and no metric" $
      render
        ( Vector
            Selector
              { selectorMetric = Nothing,
                selectorMatchers = [LabelName "l" `is` "v"],
                selectorOffset = Nothing
              }
        )
        `shouldBe` "{l=\"v\"}"

    it "writes a metric with no labels and no offset" $
      render (Vector (metric (MetricName "a"))) `shouldBe` "a"

    -- The prefix is a literal, so what is special in it has to be escaped
    -- before the ".*" that is not.
    it "escapes the literal a prefix match is built from" $
      render
        ( Vector
            (metric (MetricName "a"))
              { selectorMatchers = [LabelName "u" `matching` StartingWith "w.x"]
              }
        )
        `shouldBe` "a{u=~\"w\\\\.x.*\"}"

    it "takes a selector that names a metric and no labels" $
      isValid
        Selector
          { selectorMetric = Just (MetricName "a"),
            selectorMatchers = [],
            selectorOffset = Nothing
          }
        `shouldBe` True

  describe "more precedence" $ do
    let a = Vector (metric (MetricName "a"))

    -- A string is written where a function wanted one, and is as opaque as a
    -- name when the query around it breaks.
    it "writes a string when the query around it breaks" $
      render
        ( Binary
            Multiply
            OnEverything
            (Vector (metric (MetricName (Text.replicate 120 "a"))))
            (String (Text.replicate 20 "s"))
        )
        `shouldBe` Text.concat
          [Text.replicate 120 "a", "\n  * \"", Text.replicate 20 "s", "\""]

    -- Every operator's two sides, so that which side gets the extra step is
    -- pinned for each of them rather than for one.
    it "brackets the right-hand side of every left-associative operator" $
      map
        (\operator -> render (Binary operator OnEverything a (Binary operator OnEverything a a)))
        [Subtract, Divide, Modulo, Or, And, Unless]
        `shouldBe` [ "a - (a - a)",
                     "a / (a / a)",
                     "a % (a % a)",
                     "a or (a or a)",
                     "a and (a and a)",
                     "a unless (a unless a)"
                   ]

    it "leaves the left-hand side of every left-associative operator alone" $
      map
        (\operator -> render (Binary operator OnEverything (Binary operator OnEverything a a) a))
        [Subtract, Divide, Modulo, Or, And, Unless]
        `shouldBe` [ "a - a - a",
                     "a / a / a",
                     "a % a % a",
                     "a or a or a",
                     "a and a and a",
                     "a unless a unless a"
                   ]

    it "binds a sum more tightly than or and more loosely than a product" $
      map
        render
        [ Binary Or OnEverything (Binary Subtract OnEverything a a) a,
          Binary Subtract OnEverything (Binary Multiply OnEverything a a) a,
          Binary Multiply OnEverything (Binary Subtract OnEverything a a) a
        ]
        `shouldBe` ["a - a or a", "a * a - a", "(a - a) * a"]

  describe "layout at depth" $ do
    let as n = Text.replicate n "a"
    let wide n = Vector (metric (MetricName (as n)))

    -- The right-hand side of a left-associative operator binds one step more
    -- tightly, and that still holds once the line has given way.
    it "brackets a broken right-hand side of the same precedence" $
      render
        ( Binary
            Subtract
            OnEverything
            (wide 53)
            (Binary Subtract OnEverything (wide 60) (wide 60))
        )
        `shouldBe` Text.concat [as 53, "\n  - (", as 60, "\n       - ", as 60, ")"]

    -- A call inside a call is indented from the column the outer one left it
    -- at, not from the left margin.
    it "indents a broken call from where the call around it left off" $
      render (Apply (ClampBelow (Apply (ClampAbove (wide 120) (Number 2))) (Number 1)))
        `shouldBe` Text.concat
          ["clamp_min(\n  clamp_max(\n    ", as 120, ",\n    2\n  ),\n  1\n)"]

    -- A string has nowhere to break either, so it is written out however wide.
    it "writes a string too wide to fit rather than breaking it" $
      render (String (Text.replicate 130 "s"))
        `shouldBe` Text.concat ["\"", Text.replicate 130 "s", "\""]

    it "names the range function it was given when the call breaks" $
      render (OverTime Increase (Over (metric (MetricName (as 120))) (Minutes 5)))
        `shouldBe` Text.concat ["increase(\n  ", as 120, "[5m]\n)"]

  describe "precedence at the top" $ do
    let a = Vector (metric (MetricName "a"))

    -- Nothing is bracketed for being at the top, however loosely it binds,
    -- and the loosest of all is or.
    it "brackets nothing for being the whole query" $
      map
        render
        [ Binary Or OnEverything a a,
          Binary And OnEverything a a,
          Binary Add OnEverything a a,
          Binary Power OnEverything a a
        ]
        `shouldBe` ["a or a", "a and a", "a + a", "a ^ a"]

    -- Each operator against each neighbouring level, from the left, so that
    -- the table is pinned a step at a time rather than in one place.
    it "brackets a left-hand side that binds more loosely, at every step" $
      map
        render
        [ Binary And OnEverything (Binary Or OnEverything a a) a,
          Binary (Compare GreaterThan AsAFilter) OnEverything (Binary And OnEverything a a) a,
          Binary Add OnEverything (Binary (Compare GreaterThan AsAFilter) OnEverything a a) a,
          Binary Multiply OnEverything (Binary Add OnEverything a a) a,
          Binary Power OnEverything (Binary Multiply OnEverything a a) a
        ]
        `shouldBe` [ "(a or a) and a",
                     "(a and a) > a",
                     "(a > a) + a",
                     "(a + a) * a",
                     "(a * a) ^ a"
                   ]

    it "leaves a left-hand side that binds more tightly alone, at every step" $
      map
        render
        [ Binary Or OnEverything (Binary And OnEverything a a) a,
          Binary And OnEverything (Binary (Compare GreaterThan AsAFilter) OnEverything a a) a,
          Binary (Compare GreaterThan AsAFilter) OnEverything (Binary Add OnEverything a a) a,
          Binary Add OnEverything (Binary Multiply OnEverything a a) a,
          Binary Multiply OnEverything (Binary Power OnEverything a a) a
        ]
        `shouldBe` [ "a and a or a",
                     "a > a and a",
                     "a + a > a",
                     "a * a + a",
                     "a ^ a * a"
                   ]

  describe "validity reaches into what it holds" $ do
    it "refuses a selector whose metric name is not one" $
      isValid
        Selector
          { selectorMetric = Just (MetricName "0a"),
            selectorMatchers = [],
            selectorOffset = Nothing
          }
        `shouldBe` False

    it "refuses an expression holding a selector that is not one" $
      isValid
        ( Vector
            Selector
              { selectorMetric = Just (MetricName "0a"),
                selectorMatchers = [],
                selectorOffset = Nothing
              }
        )
        `shouldBe` False

    it "refuses an expression holding a window that is too long" $
      isValid (OverTime Rate (Over (metric (MetricName "a")) (Years 293)))
        `shouldBe` False

  -- The precedence table itself, rather than only its effect, because the
  -- brackets follow it and a step of it that is wrong is a query that means
  -- something else.  Prometheus's own table, highest binding last.
  describe "operatorPrecedence" $ do
    it "follows Prometheus's table" $
      map
        operatorPrecedence
        [ Or,
          And,
          Unless,
          Compare Equal AsAFilter,
          Add,
          Subtract,
          Multiply,
          Divide,
          Modulo,
          Atan2,
          Power
        ]
        `shouldBe` [1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 6]

    -- Nothing is bracketed for being the whole query, which is what this is
    -- for: it has to bind more loosely than anything that could be.
    it "is looser at the top than any operator" $
      lowestPrecedence `shouldSatisfy` (< operatorPrecedence Or)

    -- Nothing can come between the parts of a name or of something with its
    -- own brackets, so it has to bind more tightly than any operator.
    it "is tighter for an atom than for any operator" $
      atomPrecedence `shouldSatisfy` (> operatorPrecedence Power)

    it "says a binary expression binds as its operator does" $
      let a = Vector (metric (MetricName "a"))
       in map
            (\operator -> precedence (Binary operator OnEverything a a))
            [Or, Add, Power]
            `shouldBe` [1, 4, 6]

    it "says everything else binds as an atom" $
      let a = Vector (metric (MetricName "a"))
       in map
            precedence
            [ a,
              Number 1,
              Window (Minutes 5),
              String "s",
              Apply (Absolute a),
              Aggregate Sum Nothing a,
              OverTime Rate (Over (metric (MetricName "a")) (Minutes 5))
            ]
            `shouldBe` replicate 7 atomPrecedence

  describe "precedence between neighbours" $ do
    let a = Vector (metric (MetricName "a"))

    -- A difference of one step is the one the table is most easily wrong
    -- about, so each adjacent pair is pinned in both directions.
    it "brackets a subtraction under a sum only where it has to" $
      map
        render
        [ Binary Add OnEverything (Binary Subtract OnEverything a a) a,
          Binary Add OnEverything a (Binary Subtract OnEverything a a)
        ]
        `shouldBe` ["a - a + a", "a + (a - a)"]

    -- Exponentiation associates to the right, so its right-hand side takes
    -- anything that binds more tightly than a product without brackets and
    -- anything looser with them.
    it "brackets a product under a power" $
      map
        render
        [ Binary Power OnEverything a (Binary Multiply OnEverything a a),
          Binary Power OnEverything a (Binary Add OnEverything a a)
        ]
        `shouldBe` ["a ^ (a * a)", "a ^ (a + a)"]

  describe "layout and brackets together" $ do
    let as n = Text.replicate n "a"
    let wide n = Vector (metric (MetricName (as n)))

    -- The left-hand side of a left-associative operator binds exactly as
    -- tightly as the operator, so it goes without brackets, and that has to
    -- hold once the line has given way as well as before.
    it "leaves a broken left-hand side of the same precedence unbracketed" $
      render
        ( Binary
            Subtract
            OnEverything
            (Binary Subtract OnEverything (wide 60) (wide 60))
            (wide 53)
        )
        `shouldBe` Text.concat [as 60, "\n  - ", as 60, "\n  - ", as 53]

  -- Prometheus refuses a selector that does not narrow the series down, and
  -- it means more by that than having a matcher at all: a matcher that
  -- matches the empty string also matches every series without the label.
  describe "a selector has to narrow something" $ do
    let only matchers =
          Selector
            { selectorMetric = Nothing,
              selectorMatchers = matchers,
              selectorOffset = Nothing
            }

    it "refuses a selector of nothing but a negative matcher" $
      map
        (isValid . only . pure)
        [ LabelName "l" `isNot` "v",
          LabelName "l" `notMatching` Pattern "v"
        ]
        `shouldBe` [False, False]

    it "refuses a matcher whose value is empty" $
      isValid (only [LabelName "l" `is` ""]) `shouldBe` False

    it "refuses a regular expression that matches the empty string" $
      map
        (isValid . only . pure . matching (LabelName "l"))
        [ Literally "",
          StartingWith "",
          AnyOf ("a" :| [""])
        ]
        `shouldBe` [False, False, False]

    it "takes a matcher that cannot match a series without the label" $
      map
        (isValid . only . pure)
        [ LabelName "l" `is` "v",
          LabelName "l" `matching` Literally "v",
          LabelName "l" `matching` StartingWith "v",
          LabelName "l" `matching` AnyOf ("a" :| ["b"])
        ]
        `shouldBe` [True, True, True, True]

    -- Whether a pattern matches the empty string is a question of running a
    -- regular expression, which this library does not do, so it is taken as
    -- narrowing rather than refusing a legal query.
    it "takes a pattern at its word" $
      isValid (only [LabelName "l" `matching` Pattern ".*"]) `shouldBe` True

    it "takes a metric name as narrowing on its own" $
      isValid
        Selector
          { selectorMetric = Just (MetricName "a"),
            selectorMatchers = [LabelName "l" `isNot` "v"],
            selectorOffset = Nothing
          }
        `shouldBe` True

    it "says which matchers narrow and which do not" $
      map
        matcherNarrows
        [ LabelName "l" `is` "v",
          LabelName "l" `is` "",
          LabelName "l" `isNot` "v",
          LabelName "l" `notMatching` Pattern "v"
        ]
        `shouldBe` [True, False, False, False]

    it "says which regular expressions narrow and which do not" $
      map
        regexpNarrows
        [Literally "v", Literally "", AnyOf ("a" :| ["b"]), AnyOf ("a" :| [""]), StartingWith "v", StartingWith "", Pattern ""]
        `shouldBe` [True, False, True, False, True, False, True]

  -- A window written as something to fill in later is the one thing that
  -- reaches the query without being looked at, so what it may hold is the one
  -- thing that has to be.
  describe "a duration variable is one word" $ do
    it "takes a variable that is one word" $
      map (isValid . DurationVariable) ["$__rate_interval", "$__auto", "x"]
        `shouldBe` [True, True, True]

    it "refuses a variable that is empty, spaced, or closes the bracket" $
      map (isValid . DurationVariable) ["", "a b", "a\tb", "a]b", "a\nb"]
        `shouldBe` [False, False, False, False, False]

  describe "a string is written so it can be read back" $ do
    let valueOf text =
          render (Vector (metric (MetricName "a")) {selectorMatchers = [LabelName "l" `is` text]})

    it "escapes a backslash and a quote" $
      map valueOf ["a\\b", "a\"b"]
        `shouldBe` ["a{l=\"a\\\\b\"}", "a{l=\"a\\\"b\"}"]

    -- Prometheus takes these raw, so this is not what stops it reading the
    -- query.  A query is read back out of a dashboard's JSON and an alerting
    -- rule's YAML, where a raw control character is somebody else's problem.
    it "escapes the control characters rather than passing them through" $
      map valueOf ["a\nb", "a\rb", "a\tb"]
        `shouldBe` ["a{l=\"a\\nb\"}", "a{l=\"a\\rb\"}", "a{l=\"a\\tb\"}"]

  describe "a comparison says what it answers with" $
    it "writes the bool modifier only where it was asked for" $
      let a = Vector (metric (MetricName "a"))
       in map
            (\answering -> render (Binary (Compare GreaterThan answering) OnEverything a a))
            [AsAFilter, AsZeroOrOne]
            `shouldBe` ["a > a", "a > bool a"]
