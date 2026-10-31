{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PromQL.ParseSpec (spec) where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import PromQL
import PromQL.Gen ()
import PromQL.Parse
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec (ErrorFancy (..), ParseError (..), ShowErrorComponent (..), bundleErrors)

spec :: Spec
spec = do
  describe "parseExpr" $ do
    let a = Vector (metric (MetricName "a"))
    let b = Vector (metric (MetricName "b"))
    let c = Vector (metric (MetricName "c"))

    -- Some expressions are written alike, so what comes back is one that is
    -- written the same way rather than the same one.
    it "reads back what render writes, as something render writes the same way" $
      forAllValid $ \expr ->
        fmap render (parseExpr (render expr)) `shouldBe` Right (render expr)

    it "reads back what renderFlat writes, as the same query" $
      forAllValid $ \expr ->
        fmap render (parseExpr (renderFlat expr)) `shouldBe` Right (render expr)

    -- Every bracket the grammar allows, so this is where the precedence the
    -- parser reads is checked against the one the renderer writes.
    it "reads back what renderBracketed writes, as the same query" $
      forAllValid $ \expr ->
        fmap render (parseExpr (renderBracketed expr)) `shouldBe` Right (render expr)

    it "groups operators as Prometheus does" $
      map
        parseExpr
        [ "a + b * c",
          "a - b - c",
          "a ^ b ^ c",
          "a or b and c",
          "a > bool b",
          "a / on (x) b",
          "a * ignoring (x, y) b"
        ]
        `shouldBe` map
          Right
          [ Binary Add OnEverything a (Binary Multiply OnEverything b c),
            Binary Subtract OnEverything (Binary Subtract OnEverything a b) c,
            Binary Power OnEverything a (Binary Power OnEverything b c),
            Binary Or OnEverything a (Binary And OnEverything b c),
            Binary (Compare GreaterThan AsZeroOrOne) OnEverything a b,
            Binary Divide (On [LabelName "x"]) a b,
            Binary Multiply (Ignoring [LabelName "x", LabelName "y"]) a b
          ]

    -- A minus binds less tightly than exponentiation, so it can only be a
    -- negative number where nothing is raised to a power after it.
    it "reads a minus in front of a number as a negative number" $
      map
        parseExpr
        ["-1", "(-1) ^ 2", "2 ^ -1", "-1 * 2", "2 - -1", "+3"]
        `shouldBe` map
          Right
          [ Number (-1),
            Binary Power OnEverything (Number (-1)) (Number 2),
            Binary Power OnEverything (Number 2) (Number (-1)),
            Binary Multiply OnEverything (Number (-1)) (Number 2),
            Binary Subtract OnEverything (Number 2) (Number (-1)),
            Number 3
          ]

    it "reads numbers in each way Prometheus writes them" $
      map parseExpr ["1", "1.5", "1.0e-2", "0x1F", "1e3", ".5", "5.", ".5e1", "1E+2"]
        `shouldBe` map (Right . Number) [1, 1.5, 0.01, 31, 1000, 0.5, 5, 5, 100]

    it "reads a number that starts with its point as an operand" $
      parseExpr "a * .5" `shouldBe` Right (Binary Multiply OnEverything a (Number 0.5))

    -- Prometheus reads its keywords and aggregations in any case, and its
    -- functions only as they are written.
    it "reads keywords and aggregations in any case" $
      map
        parseExpr
        [ "a AND b",
          "a Or b",
          "a UNLESS b",
          "a > BOOL b",
          "a * ON (x) b",
          "a * IGNORING (x) b",
          "a ATAN2 b",
          "SUM BY (x) (a)",
          "Count(a) Without (x)",
          "a OFFSET 5m"
        ]
        `shouldBe` map
          Right
          [ Binary And OnEverything a b,
            Binary Or OnEverything a b,
            Binary Unless OnEverything a b,
            Binary (Compare GreaterThan AsZeroOrOne) OnEverything a b,
            Binary Multiply (On [LabelName "x"]) a b,
            Binary Multiply (Ignoring [LabelName "x"]) a b,
            Binary Atan2 OnEverything a b,
            Aggregate Sum (Just (By (LabelName "x" :| []))) a,
            Aggregate Count (Just (Without (LabelName "x" :| []))) a,
            Vector (metric (MetricName "a")) {selectorOffset = Just (Minutes 5)}
          ]

    it "reads an aggregation's grouping before or after its arguments" $
      map
        parseExpr
        ["sum by (a) (x)", "sum(x) by (a)", "sum without (a) (x)", "sum by () (x)", "topk(3, x)", "count_values(\"v\", x)"]
        `shouldBe` map
          Right
          [ Aggregate Sum (Just (By (LabelName "a" :| []))) (Vector (metric (MetricName "x"))),
            Aggregate Sum (Just (By (LabelName "a" :| []))) (Vector (metric (MetricName "x"))),
            Aggregate Sum (Just (Without (LabelName "a" :| []))) (Vector (metric (MetricName "x"))),
            Aggregate Sum Nothing (Vector (metric (MetricName "x"))),
            Aggregate (TopK (Number 3)) Nothing (Vector (metric (MetricName "x"))),
            Aggregate (CountValues (LabelName "v")) Nothing (Vector (metric (MetricName "x")))
          ]

    it "reads a selector, its window, its offset and a subquery" $
      let x = metric (MetricName "x")
       in map
            parseExpr
            [ "x{a=\"1\", b!=\"2\", c=~\"3.*\", d!~\"4\",}",
              "{a=\"1\"}",
              "x offset 5m",
              "rate(x[5m])",
              "rate(x[5m] offset 1h)",
              "max_over_time(rate(x[1m])[1h:5m])",
              "max_over_time(x[1h:])"
            ]
            `shouldBe` map
              Right
              [ Vector
                  x
                    { selectorMatchers =
                        [ LabelName "a" `is` "1",
                          LabelName "b" `isNot` "2",
                          LabelName "c" `matching` Pattern "3.*",
                          LabelName "d" `notMatching` Pattern "4"
                        ]
                    },
                Vector (series (LabelName "a" `is` "1" :| [])),
                Vector x {selectorOffset = Just (Minutes 5)},
                OverTime Rate (Over x (Minutes 5)),
                OverTime Rate (Over x {selectorOffset = Just (Hours 1)} (Minutes 5)),
                OverTime MaximumOverTime (Sampled (OverTime Rate (Over x (Minutes 1))) (Hours 1) (Just (Minutes 5))),
                OverTime MaximumOverTime (Sampled (Vector x) (Hours 1) Nothing)
              ]

    it "reads a string with each of Go's escapes, in each of Prometheus's quotes" $
      map
        parseExpr
        [ "\"a\\\"b\\\\c\\n\\t\\x41\\u00e9\\101\\012\"",
          "\"\\U0001F600\\ue000\\U0010ffff\"",
          "'it\\'s'",
          "`raw\\n`"
        ]
        `shouldBe` map
          (Right . String)
          ["a\"b\\c\n\tA\233A\n", "\128512\57344\1114111", "it's", "raw\\n"]

    -- Go reads these escapes as bytes, so a character outside ASCII is
    -- escaped as the bytes of its UTF-8.
    it "reads escaped bytes as the UTF-8 they spell" $
      map parseExpr ["\"\\xc3\\xa9\"", "\"\\303\\251\"", "'\\xC3\\xA9'", "\"a\\xc3\\xa9b\\x41\""]
        `shouldBe` map (Right . String) ["\233", "\233", "\233", "a\233bA"]

    -- A Double shows these as Prometheus's names for them do not.
    it "reads Prometheus's names for not-a-number and infinity, in any case" $
      map (fmap show . parseExpr) ["NaN", "Inf", "-inf", "nan + 1", "inf * 2"]
        `shouldBe` map
          Right
          [ "Number NaN",
            "Number Infinity",
            "Number (-Infinity)",
            "Binary Add OnEverything (Number NaN) (Number 1.0)",
            "Binary Multiply OnEverything (Number Infinity) (Number 2.0)"
          ]

    it "reads an aggregation and a function that a generated query seldom holds" $
      map parseExpr ["stdvar(a)", "clamp_max(a, 1)"]
        `shouldBe` map
          Right
          [ Aggregate StandardVariance Nothing a,
            Apply (ClampAbove a (Number 1))
          ]

    it "reads a hexadecimal number up to where its digits end" $
      parseExpr "0x1F+1" `shouldBe` Right (Binary Add OnEverything (Number 31) (Number 1))

    it "reads a word that only starts with a keyword or a number's name as a name" $
      map parseExpr ["a + online", "nanx", "info"]
        `shouldBe` map
          Right
          [ Binary Add OnEverything a (Vector (metric (MetricName "online"))),
            Vector (metric (MetricName "nanx")),
            Vector (metric (MetricName "info"))
          ]

    it "reads a metric name with colons in it, first or later" $
      map parseExpr ["job:x:rate5m", ":x"]
        `shouldBe` map
          (Right . Vector . metric . MetricName)
          ["job:x:rate5m", ":x"]

    it "reads past spaces around a label name in a grouping" $
      parseExpr "sum by ( a , b ) (x)"
        `shouldBe` Right (Aggregate Sum (Just (By (LabelName "a" :| [LabelName "b"]))) (Vector (metric (MetricName "x"))))

    it "reads a window to fill in later as an operand" $
      parseExpr "$__interval * 2"
        `shouldBe` Right (Binary Multiply OnEverything (Window (DurationVariable "$__interval")) (Number 2))

    it "refuses a string that is closed before its end" $
      isLeft (parseExpr "'it''") `shouldBe` True

    it "reads past spaces, line breaks and comments" $
      parseExpr "a # the first\n  +\n  b"
        `shouldBe` Right (Binary Add OnEverything a b)

    it "reads a window written as something to fill in later" $
      map parseExpr ["$__interval", "rate(x[$__rate_interval])"]
        `shouldBe` map
          Right
          [ Window (DurationVariable "$__interval"),
            OverTime Rate (Over (metric (MetricName "x")) (DurationVariable "$__rate_interval"))
          ]

    -- Prometheus reads each of these, and an expression here cannot hold any.
    it "refuses what an expression here cannot hold, and says which it is and where" $
      let reasons = \case
            Left bundle -> [(offset, reason) | FancyError offset errors <- NonEmpty.toList (bundleErrors bundle), ErrorCustom reason <- Set.toList errors]
            Right _ -> []
       in map
            (reasons . parseExpr)
            [ "-x",
              "-1 ^ 2",
              "a * on (x) group_left b",
              "x[1h30m]",
              "x offset -5m",
              "histogram_count(x)",
              "rate(x)",
              "abs(x[5m])",
              "x offset 5m [1h]",
              "sum(x[5m])",
              "\"\\ud800\"",
              "\"\\udfff\"",
              "\"\\U00110000\"",
              "sum(x)[5m]",
              "x[5m][1h]",
              "sum(x) offset 5m",
              "(x offset 5m) offset 1h",
              "x[5m_]",
              "ABS(x)",
              "\"\\xc3\"",
              "\"a\\xff\\x41\"",
              "\"\\400\""
            ]
            `shouldBe` map
              pure
              [ (0, NegatedNonNumber),
                (0, NegatedNonNumber),
                (11, GroupModifier),
                (4, CompoundDuration),
                (9, NegativeOffset),
                (0, UnknownFunction "histogram_count"),
                (0, WrongArguments "rate"),
                (0, WrongArguments "abs"),
                (12, OffsetBeforeWindow),
                (4, RangeOutsideFunction),
                (7, EscapeNotACharacter),
                (7, EscapeNotACharacter),
                (11, EscapeNotACharacter),
                (6, RangeOfNonSelector),
                (5, RangeOfRange),
                (7, OffsetOfNonSelector),
                (14, OffsetOfNonSelector),
                (4, CompoundDuration),
                (0, UnknownFunction "ABS"),
                (1, EscapedBytesNotUTF8),
                (2, EscapedBytesNotUTF8),
                (5, EscapeNotACharacter)
              ]

    it "refuses what Prometheus does not read either" $
      map
        (isLeft . parseExpr)
        [ "x @ 100",
          "a +",
          "sum",
          "{a=\"1\"",
          "\"a\nb\"",
          "\"\\xg0\"",
          "\"\\uzzzz\"",
          "\"\\U0000000g\"",
          "\"\\800\"",
          "\"\\108\"",
          "."
        ]
        `shouldBe` replicate 11 True

    it "says what it expected where it stopped" $
      first renderParseError (parseExpr "$")
        `shouldBe` Left
          ( unlines
              [ "1:2:",
                "  |",
                "1 | $",
                "  |  ^",
                "unexpected end of input",
                "expecting a variable's name"
              ]
          )

  describe "showErrorComponent" $
    it "names what it refuses" $
      map showErrorComponent [UnknownFunction "f", WrongArguments "rate"]
        `shouldBe` ["f is not a function an expression here can hold", "rate is not given those arguments"]

  describe "parseDuration" $ do
    it "reads back what renderDuration writes" $
      forAllValid $ \duration ->
        parseDuration (renderDuration duration) `shouldBe` Right duration

    it "reads each unit, and a variable" $
      map parseDuration ["1ms", "2s", "3m", "4h", "5d", "6w", "7y", "$__interval"]
        `shouldBe` map
          Right
          [Milliseconds 1, Seconds 2, Minutes 3, Hours 4, Days 5, Weeks 6, Years 7, DurationVariable "$__interval"]

    it "refuses a duration in more than one unit, without one, or with a fraction" $
      map (isLeft . parseDuration) ["1h30m", "5", "1.5h", "m"]
        `shouldBe` [True, True, True, True]

  describe "parseSelector" $
    it "reads a series as a promtool test file writes it" $
      map parseSelector ["up", "up{job=\"a\"}", "up {job=\"a\"}", "up offset 5m", "{}"]
        `shouldBe` map
          Right
          [ metric (MetricName "up"),
            (metric (MetricName "up")) {selectorMatchers = [LabelName "job" `is` "a"]},
            (metric (MetricName "up")) {selectorMatchers = [LabelName "job" `is` "a"]},
            (metric (MetricName "up")) {selectorOffset = Just (Minutes 5)},
            Selector
              { selectorMetric = Nothing,
                selectorMatchers = [],
                selectorOffset = Nothing
              }
          ]
