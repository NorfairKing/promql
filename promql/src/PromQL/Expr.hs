{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PromQL as a type rather than as text.
--
-- A query assembled by concatenating strings carries the grammar in its
-- author's head: the quoting of a label's value, the backslashes in a
-- matcher's regular expression, which are read twice over, and the
-- parentheses around a division, which PromQL will accept either way and
-- answer differently.  None of the three is expressible here.
--
-- Loki is the same shape as far as this goes: LogQL borrows PromQL's
-- selectors and its matchers, including how it reads a backslash.
module PromQL.Expr
  ( Expr (..),
    Aggregation (..),
    Grouping (..),
    BinOp (..),
    Comparison (..),
    Answering (..),
    isEqualTo,
    isNotEqualTo,
    isLessThan,
    isAtMost,
    isGreaterThan,
    isAtLeast,
    Matching (..),
    Call (..),
    absolute,
    roundedUp,
    roundedDown,
    roundedTo,
    exponential,
    naturalLog,
    log2,
    log10,
    squareRoot,
    sign,
    scalar,
    constantVector,
    clampBelow,
    clampAbove,
    clamp,
    absent,
    absentOverTime,
    timestamp,
    histogramQuantile,
    sorted,
    sortedDescending,
    RangeFunction (..),
    RangeVector (..),
    Replacement (..),
    Selector (..),
    LabelName (..),
    labelName,
    MetricName (..),
    metricName,
    Matcher (..),
    Match (..),
    Regexp (..),
    Duration (..),
    metric,
    series,
    matcherNarrows,
    regexpNarrows,
    is,
    isNot,
    matching,
    notMatching,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | One PromQL expression.
data Expr
  = -- | A scalar.
    Number !Double
  | -- | A window of time where a number is wanted, which is how a per-second
    -- rate is turned back into a count over that window.
    Window !Duration
  | -- | A string, which PromQL only accepts in a few places.
    String !Text
  | -- | The series a selector picks out, at the present moment.
    Vector !Selector
  | -- | A function over the samples in a window.
    OverTime !RangeFunction !RangeVector
  | -- | The quantile of the samples in a window.
    QuantileOverTime !Expr !RangeVector
  | -- | A function, with the arguments that function takes.
    Apply !Call
  | -- | A label written from another label's text.
    LabelReplace !Expr !Replacement
  | Aggregate !Aggregation !(Maybe Grouping) !Expr
  | Binary !BinOp !Matching !Expr !Expr
  deriving (Show, Eq, Generic)

-- | A number in a query is a threshold, a scale factor or a floor, and none
-- of those is a NaN or an infinity.  Prometheus can spell both, but only as
-- the bare words @NaN@ and @Inf@, which this type has no constructor for, so
-- a double that cannot be written down is one nothing here can carry.
instance Validity Expr where
  validate expr =
    mconcat
      [ genericValidate expr,
        case expr of
          Number x -> validateQueryNumber x
          _ -> valid
      ]

validateQueryNumber :: Double -> Validation
validateQueryNumber x = mconcat [validateNotNaN x, validateNotInfinite x]

data Aggregation
  = Sum
  | Average
  | Minimum
  | Maximum
  | Count
  | CountValues !LabelName
  | Group
  | StandardDeviation
  | StandardVariance
  | -- | The k largest, which takes k as well as the vector.
    TopK !Expr
  | BottomK !Expr
  | Quantile !Expr
  deriving (Show, Eq, Generic)

instance Validity Aggregation

-- | The labels an aggregation keeps, or the ones it drops.
data Grouping
  = By !(NonEmpty LabelName)
  | Without !(NonEmpty LabelName)
  deriving (Show, Eq, Generic)

instance Validity Grouping

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Power
  | Atan2
  | Compare !Comparison !Answering
  | -- | The left-hand side where it has samples, and the right-hand side
    -- where it does not.
    Or
  | And
  | Unless
  deriving (Show, Eq, Generic)

instance Validity BinOp

data Comparison
  = Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  deriving (Show, Eq, Generic)

instance Validity Comparison

-- | What a comparison answers with, which PromQL calls the bool modifier.
--
-- Two constructors rather than a 'Bool', because at a call site a bare 'True'
-- says nothing about which of the two it asked for.
data Answering
  = -- | The samples on the left that compare, and nothing else: a comparison
    -- used this way drops series rather than answering about them.
    AsAFilter
  | -- | One for each sample that compares and zero for each that does not, so
    -- that every series on the left is still there afterwards.
    AsZeroOrOne
  deriving (Show, Eq, Generic)

instance Validity Answering

-- | The comparisons that drop the samples which do not compare, which is what
-- a comparison is for unless it is being used to count.
--
-- The other half of the table is 'AsZeroOrOne', and there are no names for
-- those here: it is the rarer of the two and @Compare GreaterThan
-- AsZeroOrOne@ says what it does.
isEqualTo :: BinOp
isEqualTo = Compare Equal AsAFilter

isNotEqualTo :: BinOp
isNotEqualTo = Compare NotEqual AsAFilter

isLessThan :: BinOp
isLessThan = Compare LessThan AsAFilter

isAtMost :: BinOp
isAtMost = Compare LessOrEqual AsAFilter

isGreaterThan :: BinOp
isGreaterThan = Compare GreaterThan AsAFilter

isAtLeast :: BinOp
isAtLeast = Compare GreaterOrEqual AsAFilter

-- | Which labels the two sides of an operator are joined on.
data Matching
  = -- | The labels both sides carry, which is what an operator does unless
    -- told otherwise.
    OnEverything
  | -- | Only these labels.
    On ![LabelName]
  | -- | Every label but these.
    Ignoring ![LabelName]
  deriving (Show, Eq, Generic)

instance Validity Matching

-- | A function and its arguments.
--
-- One constructor per function, each carrying exactly the arguments that
-- function takes, so a call with the wrong number of them is not something
-- this type can hold.  A list of arguments beside a function name would be
-- the same mistake as a string: Prometheus is the one that would notice.
--
-- Not every PromQL function: the ones a dashboard or an alerting rule
-- actually asks for.  Adding one is a constructor and a line in the renderer,
-- which is the point: a query cannot name a function that does not exist.
data Call
  = Absolute !Expr
  | Ceiling !Expr
  | Floor !Expr
  | Exponential !Expr
  | NaturalLog !Expr
  | Log2 !Expr
  | Log10 !Expr
  | SquareRoot !Expr
  | Sign !Expr
  | -- | To the given number of decimal places, or to a whole number.
    Round !Expr !(Maybe Expr)
  | -- | A one-series vector as the single number it is.
    Scalar !Expr
  | -- | A number as a series, so that an expression selecting nothing still
    -- has something to answer with.
    ConstantVector !Expr
  | ClampBelow !Expr !Expr
  | ClampAbove !Expr !Expr
  | Clamp !Expr !Expr !Expr
  | Absent !Expr
  | AbsentOverTime !RangeVector
  | Timestamp !Expr
  | -- | The quantile a histogram's buckets put a reading at, which takes the
    -- buckets grouped by @le@.
    HistogramQuantile !Expr !Expr
  | Sort !Expr
  | SortDescending !Expr
  deriving (Show, Eq, Generic)

instance Validity Call

-- | Each function as the expression a call to it is, so that a caller writes
-- what it is asking for rather than the 'Apply' that puts a 'Call' back into
-- an 'Expr'.
--
-- Named after the constructor rather than after PromQL, which spells four of
-- these @abs@, @ceil@, @sgn@ and @sort_desc@: the renderer is where the
-- spellings live, and a name here that shadowed 'Prelude.abs' would be worse
-- than one that has to be looked up once.
absolute :: Expr -> Expr
absolute = Apply . Absolute

-- | PromQL's @ceil@.
roundedUp :: Expr -> Expr
roundedUp = Apply . Ceiling

-- | PromQL's @floor@.
roundedDown :: Expr -> Expr
roundedDown = Apply . Floor

-- | PromQL's @round@: to the given number of decimal places, or to a whole
-- number.
roundedTo :: Expr -> Maybe Expr -> Expr
roundedTo inner places = Apply (Round inner places)

exponential :: Expr -> Expr
exponential = Apply . Exponential

naturalLog :: Expr -> Expr
naturalLog = Apply . NaturalLog

log2 :: Expr -> Expr
log2 = Apply . Log2

log10 :: Expr -> Expr
log10 = Apply . Log10

squareRoot :: Expr -> Expr
squareRoot = Apply . SquareRoot

sign :: Expr -> Expr
sign = Apply . Sign

scalar :: Expr -> Expr
scalar = Apply . Scalar

constantVector :: Expr -> Expr
constantVector = Apply . ConstantVector

clampBelow :: Expr -> Expr -> Expr
clampBelow inner lower = Apply (ClampBelow inner lower)

clampAbove :: Expr -> Expr -> Expr
clampAbove inner upper = Apply (ClampAbove inner upper)

clamp :: Expr -> Expr -> Expr -> Expr
clamp inner lower upper = Apply (Clamp inner lower upper)

absent :: Expr -> Expr
absent = Apply . Absent

absentOverTime :: RangeVector -> Expr
absentOverTime = Apply . AbsentOverTime

timestamp :: Expr -> Expr
timestamp = Apply . Timestamp

histogramQuantile :: Expr -> Expr -> Expr
histogramQuantile quantile buckets = Apply (HistogramQuantile quantile buckets)

sorted :: Expr -> Expr
sorted = Apply . Sort

sortedDescending :: Expr -> Expr
sortedDescending = Apply . SortDescending

data RangeFunction
  = -- | Per-second rate of increase, which is what a counter is read with.
    Rate
  | InstantRate
  | -- | How much a counter went up over the window.
    Increase
  | -- | How much a gauge moved over the window.
    Delta
  | InstantDelta
  | Derivative
  | -- | How many times a gauge changed over the window.
    Changes
  | Resets
  | AverageOverTime
  | MinimumOverTime
  | MaximumOverTime
  | SumOverTime
  | CountOverTime
  | LastOverTime
  | PresentOverTime
  | StandardDeviationOverTime
  | StandardVarianceOverTime
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity RangeFunction

-- | The samples a range function reads.
data RangeVector
  = -- | One selector's samples over a window.
    Over !Selector !Duration
  | -- | An expression evaluated repeatedly over a window, which is what
    -- PromQL calls a subquery: the second duration is how often.
    Sampled !Expr !Duration !(Maybe Duration)
  deriving (Show, Eq, Generic)

instance Validity RangeVector

-- | What @label_replace@ is told: the label to write, the text to write into
-- it, the label to read, and the expression that picks the part to keep.
data Replacement = Replacement
  { replacementDestination :: !LabelName,
    replacementText :: !Text,
    replacementSource :: !LabelName,
    replacementRegexp :: !Regexp
  }
  deriving (Show, Eq, Generic)

instance Validity Replacement

-- | The series a query is about.
data Selector = Selector
  { -- | The metric's name, which a selector over nothing but labels does not
    -- have.  Loki's selectors are all of that kind.
    selectorMetric :: !(Maybe MetricName),
    selectorMatchers :: ![Matcher],
    -- | How far back to read instead of now, for comparing a reading with the
    -- one before it.
    selectorOffset :: !(Maybe Duration)
  }
  deriving (Show, Eq, Generic)

-- | A selector has to narrow the series down to something.
--
-- Prometheus refuses one that does not, and it means more by that than having
-- a matcher at all: a matcher that matches the empty string also matches
-- every series that does not carry the label, so a selector made only of
-- those still asks for everything.  @{l!="v"}@ and @{l=""}@ are both refused
-- for that reason.
instance Validity Selector where
  validate selector =
    mconcat
      [ genericValidate selector,
        declare "the selector narrows the series down to something" $
          isJust (selectorMetric selector)
            || any matcherNarrows (selectorMatchers selector)
      ]

-- | Whether a matcher is one Prometheus counts as narrowing the selection,
-- which is to say one that cannot match a series the label is absent from.
matcherNarrows :: Matcher -> Bool
matcherNarrows matcher = case matcherMatch matcher of
  Is value -> not (Text.null value)
  -- A negative matcher matches every series without the label, so it narrows
  -- nothing on its own.
  IsNot _ -> False
  Matches regexp -> regexpNarrows regexp
  DoesNotMatch _ -> False

-- | The same question of a regular expression: whether it is one that cannot
-- match the empty string.
regexpNarrows :: Regexp -> Bool
regexpNarrows = \case
  Literally literal -> not (Text.null literal)
  -- One empty alternative is enough to match the empty string.
  AnyOf literals -> not (any Text.null literals)
  StartingWith prefix -> not (Text.null prefix)
  -- A regular expression nothing here runs, so whether it matches the empty
  -- string is Prometheus's to say.  Taken as narrowing: a pattern that does
  -- not is the rarer thing, and refusing one that does would turn a legal
  -- query, such as the @{unit=~"prefix.+"}@ a log stream is picked out with,
  -- into a value this type will not hold.
  Pattern _ -> True

-- | A selector for a metric, with nothing said about its labels yet.
metric :: MetricName -> Selector
metric name =
  Selector
    { selectorMetric = Just name,
      selectorMatchers = [],
      selectorOffset = Nothing
    }

-- | A selector for whatever carries these labels, with no metric name, which
-- is how a Loki stream is picked out.
series :: NonEmpty Matcher -> Selector
series matchers =
  Selector
    { selectorMetric = Nothing,
      selectorMatchers = NE.toList matchers,
      selectorOffset = Nothing
    }

data Matcher = Matcher
  { matcherLabel :: !LabelName,
    matcherMatch :: !Match
  }
  deriving (Show, Eq, Generic)

instance Validity Matcher

data Match
  = Is !Text
  | IsNot !Text
  | Matches !Regexp
  | DoesNotMatch !Regexp
  deriving (Show, Eq, Generic)

instance Validity Match

is :: LabelName -> Text -> Matcher
is label value =
  Matcher
    { matcherLabel = label,
      matcherMatch = Is value
    }

isNot :: LabelName -> Text -> Matcher
isNot label value =
  Matcher
    { matcherLabel = label,
      matcherMatch = IsNot value
    }

matching :: LabelName -> Regexp -> Matcher
matching label regexp =
  Matcher
    { matcherLabel = label,
      matcherMatch = Matches regexp
    }

notMatching :: LabelName -> Regexp -> Matcher
notMatching label regexp =
  Matcher
    { matcherLabel = label,
      matcherMatch = DoesNotMatch regexp
    }

-- | A regular expression as a matcher holds one.
--
-- A matcher's value is a string that is then read as a regular expression, so
-- a backslash is read twice and a dot that is meant to be a dot has to
-- survive both readings.  Nobody should have to count backslashes, which is
-- what the first three of these are for: they take the text that is meant and
-- escape it.
--
-- 'Pattern' is for where a regular expression is what is meant.  It escapes
-- nothing, which is the point, and it is the one way to get a character class
-- or an alternation of patterns into a query.
data Regexp
  = -- | Exactly this text and nothing else.
    Literally !Text
  | -- | Any one of these, exactly.
    AnyOf !(NonEmpty Text)
  | -- | This text and anything after it.
    StartingWith !Text
  | -- | A regular expression, written as one.
    Pattern !Text
  deriving (Show, Eq, Generic)

instance Validity Regexp

-- | A window of time, as PromQL writes one.
data Duration
  = Milliseconds !Word
  | Seconds !Word
  | Minutes !Word
  | Hours !Word
  | Days !Word
  | Weeks !Word
  | Years !Word
  | -- | Something only the thing rendering the query knows how to fill in,
    -- such as one of Grafana's @$__rate_interval@ and friends.  Rendered as
    -- it is given, because what it stands for is not PromQL's business.
    DurationVariable !Text
  deriving (Show, Eq, Generic)

-- | Prometheus reads a duration into a 64-bit count of nanoseconds, so one
-- longer than that span is not a duration it has anywhere to put.  The bounds
-- are per unit because that is how one is written.
-- Written without 'genericValidate': every field here is a 'Word' or a
-- 'Text', whose validity is trivial, so it would add nothing and the mutation
-- testing says as much.
instance Validity Duration where
  validate duration =
    mconcat
      [ declare "a window written as something to fill in later is one word" $
          case duration of
            DurationVariable variable ->
              not (Text.null variable)
                && Text.all (\character -> not (isSpace character) && character /= ']') variable
            _ -> True,
        declare "the duration is one Prometheus has somewhere to put" $
          case duration of
            Milliseconds n -> n <= 9223372036854
            Seconds n -> n <= 9223372036
            Minutes n -> n <= 153722867
            Hours n -> n <= 2562047
            Days n -> n <= 106751
            Weeks n -> n <= 15250
            Years n -> n <= 292
            DurationVariable _ -> True
      ]

-- | A label's name, which Prometheus spells like an identifier.
--
-- A newtype rather than 'Text' because a matcher on a label whose name is not
-- one is not a query Prometheus will read, and the rendered text gives no
-- sign of it: the complaint comes back from the server as a parse error
-- pointing inside a brace.
--
-- The constructor is exported because building a name out of text that is
-- already known to be one is the common case. 'labelName' is for text that
-- is not known to be one, and 'validate' says which it is.
newtype LabelName = LabelName {unLabelName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity LabelName where
  validate name =
    declare "the name is one Prometheus will read as a label name" $
      isIdentifier (unLabelName name)

-- | A label name, or nothing where the text is not one.
labelName :: Text -> Maybe LabelName
labelName text = constructValid (LabelName text)

-- | A metric's name, which is a label name that may also carry colons: those
-- are reserved for recording rules, which is why nothing generates one.
newtype MetricName = MetricName {unMetricName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity MetricName where
  validate name =
    declare "the name is one Prometheus will read as a metric name" $
      isMetricIdentifier (unMetricName name)

metricName :: Text -> Maybe MetricName
metricName text = constructValid (MetricName text)

-- | What Prometheus reads as a name: a letter or an underscore, and then
-- letters, digits and underscores.
isIdentifier :: Text -> Bool
isIdentifier text = case Text.uncons text of
  Nothing -> False
  Just (first, rest) -> startsName first && Text.all continuesName rest

-- | The same, except that a metric name may also contain colons.
isMetricIdentifier :: Text -> Bool
isMetricIdentifier text = case Text.uncons text of
  Nothing -> False
  Just (first, rest) ->
    (startsName first || first == ':')
      && Text.all (\character -> continuesName character || character == ':') rest

startsName :: Char -> Bool
startsName character = isAsciiUpper character || isAsciiLower character || character == '_'

continuesName :: Char -> Bool
continuesName character = startsName character || isDigit character
