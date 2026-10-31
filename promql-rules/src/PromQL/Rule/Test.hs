{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for rules, in the format @promtool test rules@ reads.
--
-- A test says what series existed and what they held at each step, and then
-- what an alert or an expression should have answered at some moment.
-- promtool evaluates the rules over those series with Prometheus's own
-- engine, so the answers are Prometheus's rather than a transcription of
-- them.
--
-- The codecs and the renderer are left out of mutation testing: the golden
-- file of what they write fails on any change to it, and the roundtrip
-- properties on any change to what they read.
module PromQL.Rule.Test
  ( RuleTestFile (..),
    RuleTest (..),
    InputSeries (..),
    Series (..),
    Segment (..),
    Ramp (..),
    sample,
    constant,
    Sample (..),
    expandSegments,
    renderSegments,
    renderSeries,
    parseSeries,
    SeriesError (..),
    renderSeriesError,
    parseSegments,
    AlertTest (..),
    alertTest,
    alertsWithoutRules,
    ExpectedAlert (..),
    ExprTest (..),
    ExpectedSample (..),
    renderRuleTestFile,
  )
where

import Autodocodec
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.List (genericReplicate, genericTake, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Scientific (fromFloatDigits, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Void (Void)
import GHC.Generics (Generic)
import Path (File, SomeBase, fromSomeFile)
import PromQL
import PromQL.Codec ()
import PromQL.Parse (ParseError, parseSelector, renderParseError)
import PromQL.Rule
import PromQL.Rule.Fields
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, eof, errorBundlePretty, many, option, parse)
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- | What one test file holds, apart from which rule files it tests.
data RuleTestFile = RuleTestFile
  { -- | How often the rules are evaluated, where it is not promtool's default
    -- of a minute.
    ruleTestFileEvaluationInterval :: !(Maybe Duration),
    ruleTestFileTests :: ![RuleTest]
  }
  deriving (Show, Eq, Generic)

instance Validity RuleTestFile where
  validate testFile =
    mconcat
      [ genericValidate testFile,
        foldMap validateFixedDuration (ruleTestFileEvaluationInterval testFile)
      ]

instance HasCodec RuleTestFile where
  codec = ruleTestFileCodec

{-# ANN ruleTestFileCodec ("DisableMutations" :: String) #-}
ruleTestFileCodec :: JSONCodec RuleTestFile
ruleTestFileCodec = bimapCodec prettyValidate id (object "RuleTestFile" ruleTestFileObjectCodec)

-- | The fields of a test file besides the rule files it tests, which
-- 'renderRuleTestFile' adds.
{-# ANN ruleTestFileObjectCodec ("DisableMutations" :: String) #-}
ruleTestFileObjectCodec :: JSONObjectCodec RuleTestFile
ruleTestFileObjectCodec =
  RuleTestFile
    <$> optionalField "evaluation_interval" "How often the rules are evaluated" .= ruleTestFileEvaluationInterval
    <*> optionalFieldWithDefault "tests" [] "The tests" .= ruleTestFileTests

-- | One scenario: some series, and what the rules should make of them.
data RuleTest = RuleTest
  { -- | What promtool calls the test when it fails.
    ruleTestName :: !Text,
    -- | How far apart the steps of the input series are, where it is not
    -- the evaluation interval, which is what promtool takes by default.
    ruleTestInterval :: !(Maybe Duration),
    -- | The labels Prometheus would add to every alert it sends, which a
    -- template can read as @$externalLabels@.
    ruleTestExternalLabels :: !(Map LabelName Text),
    ruleTestInputSeries :: ![InputSeries],
    ruleTestAlertTests :: ![AlertTest],
    ruleTestExprTests :: ![ExprTest]
  }
  deriving (Show, Eq, Generic)

instance Validity RuleTest where
  validate ruleTest =
    mconcat
      [ genericValidate ruleTest,
        foldMap validateFixedDuration (ruleTestInterval ruleTest),
        -- promtool keeps only the last of two input series that are the
        -- same series, without saying so.
        declare "no two input series are the same series" $
          let series' = map inputSeriesSeries (ruleTestInputSeries ruleTest)
           in nub series' == series'
      ]

instance HasCodec RuleTest where
  codec = ruleTestCodec

{-# ANN ruleTestCodec ("DisableMutations" :: String) #-}
ruleTestCodec :: JSONCodec RuleTest
ruleTestCodec =
  validatedObject "RuleTest" $
    RuleTest
      <$> requiredField "name" "What promtool calls the test" .= ruleTestName
      <*> optionalField "interval" "How far apart the steps of the input series are" .= ruleTestInterval
      <*> labelMapField "external_labels" "The labels Prometheus adds to every alert" .= ruleTestExternalLabels
      <*> optionalFieldWithDefault "input_series" [] "The series" .= ruleTestInputSeries
      <*> optionalFieldWithDefault "alert_rule_test" [] "What the alerts should be doing" .= ruleTestAlertTests
      <*> optionalFieldWithDefault "promql_expr_test" [] "What expressions should answer" .= ruleTestExprTests

-- | A series and what it held, one step after the other, from time zero.
data InputSeries = InputSeries
  { inputSeriesSeries :: !Series,
    inputSeriesValues :: ![Segment]
  }
  deriving (Show, Eq, Generic)

-- | promtool refuses an input series with neither a name nor labels, which
-- is a series that only an expected scalar is compared with.
instance Validity InputSeries where
  validate input =
    mconcat
      [ genericValidate input,
        declare "the series has a name or a label" $
          let series' = inputSeriesSeries input
           in isJust (seriesMetric series') || not (Map.null (seriesLabels series'))
      ]

instance HasCodec InputSeries where
  codec = inputSeriesCodec

{-# ANN inputSeriesCodec ("DisableMutations" :: String) #-}
inputSeriesCodec :: JSONCodec InputSeries
inputSeriesCodec =
  validatedObject "InputSeries" $
    InputSeries
      <$> requiredField "series" "The series" .= inputSeriesSeries
      <*> requiredFieldWith "values" (bimapCodec (first errorBundlePretty . parseSegments) renderSegments textCodec) "What it held, in promtool's notation" .= inputSeriesValues

-- | Exactly one series: a metric's name, if it has one, and every label.
--
-- Not a 'Selector': a selector picks out series, and this is one of them, so
-- each label has one value and that value is not empty.
-- Prometheus reads an empty label value as a label that is not there, so
-- allowing one would give the same series two spellings.
data Series = Series
  { seriesMetric :: !(Maybe MetricName),
    seriesLabels :: !(Map LabelName Text)
  }
  deriving (Show, Eq, Generic)

instance Validity Series where
  validate series' =
    mconcat
      [ genericValidate series',
        declare "no label value is empty" $
          not (any Text.null (seriesLabels series')),
        declare "the metric name is not among the labels" $
          not (Map.member (LabelName "__name__") (seriesLabels series')),
        foldMap validateQueryText (seriesLabels series')
      ]

instance HasCodec Series where
  codec = seriesCodec

seriesCodec :: JSONCodec Series
seriesCodec = bimapCodec (\text -> first renderSeriesError (parseSeries text) >>= prettyValidate) renderSeries textCodec <?> "A series, written as a selector for exactly it"

-- | Some consecutive steps of a series.
--
-- Each counts the steps it covers.
-- promtool's own notation does not do so consistently: @1x3@ is four samples
-- while @_x3@ is three gaps, and this type is where that stops mattering.
data Segment
  = -- | A sample at each step, going up or down by the same amount each time.
    Samples !Ramp
  | -- | Steps at which the series has no sample.
    Missing !Word
  | -- | A staleness marker, which is how Prometheus records that a series
    -- went away, rather than only that it was not scraped.
    Stale
  deriving (Show, Eq, Generic)

instance Validity Segment where
  validate segment =
    mconcat
      [ genericValidate segment,
        declare "a run of missing steps covers at least one" $ case segment of
          Missing count -> count >= 1
          Samples _ -> True
          Stale -> True
      ]

instance HasCodec Segment where
  codec = segmentCodec

{-# ANN segmentCodec ("DisableMutations" :: String) #-}
segmentCodec :: JSONCodec Segment
segmentCodec =
  bimapCodec
    ( \text -> case parseSegments text of
        Left bundle -> Left (errorBundlePretty bundle)
        Right [segment] -> prettyValidate segment
        Right segments -> Left (unwords ["one segment, not", show (length segments)])
    )
    renderSegment
    textCodec
    <?> "Some steps of a series, in promtool's notation"

-- | Samples that start somewhere and change by the same amount at each step.
data Ramp = Ramp
  { rampStart :: !Double,
    -- | What is added at each step, which may be negative or zero.
    rampIncrement :: !Double,
    -- | How many samples, the first included.
    rampCount :: !Word
  }
  deriving (Show, Eq, Generic)

-- | A ramp holds only numbers that can be written down and compared: the
-- file a test is written to cannot carry a NaN or an infinity as an
-- expected value, so a series that holds one could not be tested against.
--
-- Every sample is checked rather than only the start, because a ramp of
-- finite numbers can climb past the largest one there is.
-- The increment is checked on its own as well, because a ramp of one sample
-- never adds it.
instance Validity Ramp where
  validate ramp =
    mconcat
      [ declare "the increment is a finite number" $ isFinite (rampIncrement ramp),
        declare "a ramp covers at least one step" $ rampCount ramp >= 1,
        declare "every sample is a finite number" $ all isFinite (rampSamples ramp)
      ]

instance HasCodec Ramp where
  codec = rampCodec

rampCodec :: JSONCodec Ramp
rampCodec = bimapCodec (\text -> first errorBundlePretty (parseWith rampParser text) >>= prettyValidate) renderRamp textCodec <?> "Samples that change by the same amount at each step"

-- | A ramp's samples, summed one step after another rather than multiplied
-- out, because that is what promtool does, and the two disagree in the last
-- digits of a fractional increment.
rampSamples :: Ramp -> [Double]
rampSamples ramp =
  genericTake (rampCount ramp) (iterate (+ rampIncrement ramp) (rampStart ramp))

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)

-- | One step, with this sample.
sample :: Double -> Segment
sample value = constant value 1

-- | This many steps, each with the same sample.
constant :: Double -> Word -> Segment
constant value count =
  Samples
    Ramp
      { rampStart = value,
        rampIncrement = 0,
        rampCount = count
      }

-- | What a series holds at one step.
data Sample
  = SampleValue !Double
  | SampleMissing
  | SampleStale
  deriving (Show, Eq)

-- | What the segments mean, one step at a time.
expandSegments :: [Segment] -> [Sample]
expandSegments = concatMap $ \case
  Samples ramp ->
    map SampleValue (rampSamples ramp)
  Missing count -> genericReplicate count SampleMissing
  Stale -> [SampleStale]

-- | The segments in promtool's expanding notation.
renderSegments :: [Segment] -> Text
renderSegments = Text.unwords . map renderSegment

renderSegment :: Segment -> Text
renderSegment = \case
  Samples ramp -> renderRamp ramp
  Missing 1 -> "_"
  -- promtool counts every missing step, unlike the samples.
  Missing count -> Text.concat ["_x", Text.pack (show count)]
  Stale -> "stale"

renderRamp :: Ramp -> Text
renderRamp ramp =
  let start = renderNumber (rampStart ramp)
      increment = rampIncrement ramp
      -- promtool counts the steps after the first one.
      times = Text.pack (show (rampCount ramp - 1))
   in case (rampCount ramp, compare increment 0) of
        (1, _) -> start
        (_, EQ) -> Text.concat [start, "x", times]
        (_, LT) -> Text.concat [start, "-", renderNumber (negate increment), "x", times]
        (_, GT) -> Text.concat [start, "+", renderNumber increment, "x", times]

-- | What an alert should be doing at one moment.
data AlertTest = AlertTest
  { -- | How long after time zero to look.
    alertTestEvalTime :: !Duration,
    alertTestAlertName :: !AlertName,
    -- | Every alert of that name that should be firing, and no more: one
    -- that is only pending is not firing, so an empty list says that nothing
    -- has fired yet.
    alertTestExpectedAlerts :: ![ExpectedAlert]
  }
  deriving (Show, Eq, Generic)

instance Validity AlertTest where
  validate test =
    mconcat
      [ genericValidate test,
        validateFixedDuration (alertTestEvalTime test),
        -- An alert is its labels, so two with the same labels are one alert,
        -- and a test expecting both could never pass.
        declare "no two expected alerts have the same labels" $
          let labels = map expectedAlertLabels (alertTestExpectedAlerts test)
           in nub labels == labels
      ]

instance HasCodec AlertTest where
  codec = alertTestCodec

{-# ANN alertTestCodec ("DisableMutations" :: String) #-}
alertTestCodec :: JSONCodec AlertTest
alertTestCodec =
  validatedObject "AlertTest" $
    AlertTest
      <$> requiredField "eval_time" "How long after time zero to look" .= alertTestEvalTime
      <*> requiredField "alertname" "The alert" .= alertTestAlertName
      <*> optionalFieldWithDefault "exp_alerts" [] "Every alert of that name that should be firing" .= alertTestExpectedAlerts

-- | A test of this rule, so that the name being tested is the name of a rule
-- rather than text that has to be kept the same as one.
alertTest :: AlertingRule -> Duration -> [ExpectedAlert] -> AlertTest
alertTest rule evalTime expected =
  AlertTest
    { alertTestEvalTime = evalTime,
      alertTestAlertName = alertingRuleName rule,
      alertTestExpectedAlerts = expected
    }

-- | The alerts that are tested but that no alerting rule in the file defines.
--
-- promtool passes a test of such an alert as long as it expects nothing to
-- fire, so the test would pass whatever the rules said.
alertsWithoutRules :: RuleFile -> RuleTestFile -> [AlertName]
alertsWithoutRules ruleFile testFile =
  let defined =
        Set.fromList
          [ alertingRuleName rule
          | group <- ruleFileGroups ruleFile,
            Alerting rule <- ruleGroupRules group
          ]
      tested =
        Set.fromList
          [ alertTestAlertName test
          | ruleTest <- ruleTestFileTests testFile,
            test <- ruleTestAlertTests ruleTest
          ]
   in Set.toList (Set.difference tested defined)

-- | One firing alert, as Prometheus would send it.
data ExpectedAlert = ExpectedAlert
  { -- | Every label the alert carries: the series' own and the rule's, but
    -- not @alertname@, which promtool adds itself.
    expectedAlertLabels :: !(Map LabelName Text),
    -- | Every annotation, with its template already expanded.
    expectedAlertAnnotations :: !(Map LabelName Text)
  }
  deriving (Show, Eq, Generic)

instance Validity ExpectedAlert

instance HasCodec ExpectedAlert where
  codec = expectedAlertCodec

{-# ANN expectedAlertCodec ("DisableMutations" :: String) #-}
expectedAlertCodec :: JSONCodec ExpectedAlert
expectedAlertCodec =
  validatedObject "ExpectedAlert" $
    ExpectedAlert
      <$> labelMapField "exp_labels" "Every label the alert carries but alertname" .= expectedAlertLabels
      <*> labelMapField "exp_annotations" "Every annotation, expanded" .= expectedAlertAnnotations

-- | What an expression should answer with at one moment.
--
-- The expression may use what the rules record, which is how a recording
-- rule is tested.
data ExprTest = ExprTest
  { exprTestExpr :: !Expr,
    exprTestEvalTime :: !Duration,
    -- | Every sample it should answer with, in any order.
    exprTestExpectedSamples :: ![ExpectedSample]
  }
  deriving (Show, Eq, Generic)

instance Validity ExprTest where
  validate test =
    mconcat
      [ genericValidate test,
        validateFixedDuration (exprTestEvalTime test),
        -- An expression answers with each series at most once.
        declare "no two expected samples are of the same series" $
          let series' = map expectedSampleSeries (exprTestExpectedSamples test)
           in nub series' == series'
      ]

instance HasCodec ExprTest where
  codec = exprTestCodec

{-# ANN exprTestCodec ("DisableMutations" :: String) #-}
exprTestCodec :: JSONCodec ExprTest
exprTestCodec =
  validatedObject "ExprTest" $
    ExprTest
      <$> requiredField "expr" "The expression" .= exprTestExpr
      <*> requiredField "eval_time" "How long after time zero to look" .= exprTestEvalTime
      <*> optionalFieldWithDefault "exp_samples" [] "Every sample it should answer with" .= exprTestExpectedSamples

data ExpectedSample = ExpectedSample
  { -- | The series, which is one with neither a name nor labels where the
    -- expression answers with a scalar.
    expectedSampleSeries :: !Series,
    expectedSampleValue :: !Double
  }
  deriving (Show, Eq, Generic)

instance Validity ExpectedSample where
  validate expected =
    mconcat
      [ genericValidate expected,
        declare "the value is a finite number" $ isFinite (expectedSampleValue expected)
      ]

instance HasCodec ExpectedSample where
  codec = expectedSampleCodec

{-# ANN expectedSampleCodec ("DisableMutations" :: String) #-}
expectedSampleCodec :: JSONCodec ExpectedSample
expectedSampleCodec =
  validatedObject "ExpectedSample" $
    ExpectedSample
      <$> requiredField "labels" "The series" .= expectedSampleSeries
      <*> requiredFieldWith "value" (dimapCodec toRealFloat fromFloatDigits scientificCodec) "The value" .= expectedSampleValue

-- | A test file as promtool reads one, testing the rule files at these
-- paths.
--
-- A relative path is read relative to the test file, not to wherever
-- promtool is run from.
{-# ANN renderRuleTestFile ("DisableMutations" :: String) #-}
renderRuleTestFile :: [SomeBase File] -> RuleTestFile -> ByteString
renderRuleTestFile ruleFiles =
  renderFile $
    object "PromtoolTestFile" $
      (requiredFieldWith' "rule_files" (listCodec textCodec) .= const (map (Text.pack . fromSomeFile) ruleFiles))
        *> ruleTestFileObjectCodec

-- | One series, written the way a selector for exactly it would be, which is
-- how promtool reads one.
--
-- A series with neither a name nor labels is written as empty braces, which
-- is what a scalar's answer is compared with.
renderSeries :: Series -> Text
renderSeries series' = case (seriesMetric series', Map.toList (seriesLabels series')) of
  (Nothing, []) -> "{}"
  (name, pairs) ->
    renderSelector
      Selector
        { selectorMetric = name,
          selectorMatchers = map (uncurry is) pairs,
          selectorOffset = Nothing
        }

-- | Why some text is not a series.
data SeriesError
  = -- | It is not a selector at all.
    SeriesNotASelector !ParseError
  | -- | It is a selector with an offset, which a series does not have.
    SeriesOffset
  | -- | It matches this label other than by its one value.
    SeriesNotExact !LabelName
  | -- | It gives this label more than one value.
    SeriesLabelRepeated !LabelName
  deriving (Show, Eq)

renderSeriesError :: SeriesError -> String
renderSeriesError = \case
  SeriesNotASelector parseError -> renderParseError parseError
  SeriesOffset -> "a series has no offset"
  SeriesNotExact name -> unwords ["a series has one value for", show (unLabelName name)]
  SeriesLabelRepeated name -> unwords ["a series has one value for", show (unLabelName name), "and this gives it more"]

-- | A series as 'renderSeries' writes one: a selector with nothing but
-- exact matchers, each on a different label.
parseSeries :: Text -> Either SeriesError Series
parseSeries text = do
  let exactly :: Matcher -> Either SeriesError (LabelName, Text)
      exactly matcher = case matcherMatch matcher of
        Is value -> Right (matcherLabel matcher, value)
        IsNot _ -> Left (SeriesNotExact (matcherLabel matcher))
        Matches _ -> Left (SeriesNotExact (matcherLabel matcher))
        DoesNotMatch _ -> Left (SeriesNotExact (matcherLabel matcher))
  let add :: Map LabelName Text -> (LabelName, Text) -> Either SeriesError (Map LabelName Text)
      add labelMap (name, value)
        | Map.member name labelMap = Left (SeriesLabelRepeated name)
        | otherwise = Right (Map.insert name value labelMap)
  selector <- first SeriesNotASelector (parseSelector text)
  labels <- traverse exactly (selectorMatchers selector)
  labelMap <- foldM add Map.empty labels
  case selectorOffset selector of
    Just _ -> Left SeriesOffset
    Nothing ->
      Right
        Series
          { seriesMetric = selectorMetric selector,
            seriesLabels = labelMap
          }

-- | Segments as 'renderSegments' writes them, and as promtool reads them.
--
-- What comes back is the same steps, but not always the same segments: a
-- ramp of one sample is written as its number, whatever it would have added.
parseSegments :: Text -> Either (ParseErrorBundle Text Void) [Segment]
parseSegments = parseWith (space *> many (segmentParser <* space))

parseWith :: Parsec Void Text a -> Text -> Either (ParseErrorBundle Text Void) a
parseWith parser = parse (parser <* eof) ""

segmentParser :: Parsec Void Text Segment
segmentParser =
  choice
    [ Stale <$ string "stale",
      char '_' *> (Missing <$> option 1 (char 'x' *> (Lexer.decimal >>= stepCount))),
      Samples <$> rampParser
    ]

-- | A number, then what is added at each step and how many steps after the
-- first there are, if there are any.
--
-- Not 'PromQL.Parse.numberParser': promtool reads @0x3@ as zero and three
-- more steps of it, where a query would read a hexadecimal three.
rampParser :: Parsec Void Text Ramp
rampParser = do
  let unsignedNumber :: Parsec Void Text Double
      unsignedNumber = toRealFloat <$> Lexer.scientific
  start <- option id (negate <$ char '-') <*> unsignedNumber
  let ramp :: Double -> Integer -> Parsec Void Text Ramp
      ramp increment times = do
        count' <- stepCount (times + 1)
        pure
          Ramp
            { rampStart = start,
              rampIncrement = increment,
              rampCount = count'
            }
  choice
    [ do
        direction <- choice [id <$ char '+', negate <$ char '-']
        increment <- unsignedNumber
        times <- char 'x' *> Lexer.decimal
        ramp (direction increment) times,
      char 'x' *> Lexer.decimal >>= ramp 0,
      ramp 0 0
    ]

-- | A number of steps, refused rather than wrapped around where it is more
-- than a 'Word' holds.
stepCount :: Integer -> Parsec Void Text Word
stepCount n
  | n > toInteger (maxBound :: Word) = fail "more steps than can be counted"
  | otherwise = pure (fromInteger n)
