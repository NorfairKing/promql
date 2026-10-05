{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators for the PromQL types.
--
-- A separate package from the one that defines them, so that nothing a query
-- is built with has to carry QuickCheck.
--
-- PromQL has a type system that "PromQL.Expr" does not: @log2@ takes an
-- instant vector and will not take a scalar, and @clamp_min@ takes a vector
-- and a scalar in that order.  'Expr' can hold either, so it is these
-- generators that know the rules, and an expression built by hand can still
-- be one Prometheus refuses.  Making that unrepresentable means indexing
-- 'Expr' by what it evaluates to, which is a different type and a much larger
-- one; until then this is where the rules are written down.
module PromQL.Gen
  ( genIdentifier,
    genScalar,
    genInstantVector,
    genRange,
    genDurationVariable,
  )
where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import qualified Data.Text as Text
import PromQL
import Test.QuickCheck

-- | A well-typed expression, which is to say one Prometheus will take.
--
-- An instant vector, because that is what a query asks for: the scalars and
-- the range vectors in here are the ones some function wanted.
instance GenValid Expr where
  genValid = genInstantVector

  -- Structural shrinking can walk out of the well-typed expressions and into
  -- the merely valid ones, so a shrunk counterexample is sometimes one
  -- Prometheus would refuse for a different reason than the original.  That
  -- is a worse report rather than a wrong one, and it only happens on the way
  -- to explaining a failure.
  shrinkValid = filter isValid . shrinkValidStructurally

-- | A single number.
genScalar :: Gen Expr
genScalar =
  sized $ \size ->
    if size <= 1
      then oneof [Number <$> genQueryNumber, Window <$> genValid]
      else
        oneof
          [ Number <$> genQueryNumber,
            Window <$> genValid,
            Apply . Scalar <$> half genInstantVector,
            Binary
              <$> genScalarArithmetic
              <*> pure OnEverything
              <*> half genScalar
              <*> half genScalar
          ]

-- | The series an expression answers with, which is what a query is.
genInstantVector :: Gen Expr
genInstantVector =
  sized $ \size ->
    if size <= 1
      then Vector <$> genValid
      else
        oneof
          [ Vector <$> genValid,
            OverTime <$> genValid <*> half genRange,
            QuantileOverTime <$> half genScalar <*> half genRange,
            Apply <$> half genCall,
            LabelReplace <$> half genInstantVector <*> genValid,
            Aggregate
              <$> half genAggregation
              <*> genValid
              <*> half genInstantVector,
            -- A vector against a vector, or a vector against a scalar, which
            -- are the two an operator takes.  Scalar against scalar is a
            -- scalar and belongs in 'genScalar'.
            Binary
              <$> genArithmetic
              -- Vector matching is only allowed between two vectors, so an
              -- operator with a scalar on one side joins on nothing.
              <*> pure OnEverything
              <*> half genInstantVector
              <*> half genScalar,
            Binary
              <$> genArithmetic
              <*> genValid
              <*> half genInstantVector
              <*> half genInstantVector,
            -- Set operators take a vector on both sides and nothing else.
            Binary
              <$> elements [And, Or, Unless]
              <*> genValid
              <*> half genInstantVector
              <*> half genInstantVector
          ]

genCall :: Gen Call
genCall =
  oneof
    [ overAVector Absolute,
      overAVector Ceiling,
      overAVector Floor,
      overAVector Exponential,
      overAVector NaturalLog,
      overAVector Log2,
      overAVector Log10,
      overAVector SquareRoot,
      overAVector Sign,
      overAVector Absent,
      overAVector Timestamp,
      overAVector Sort,
      overAVector SortDescending,
      Round <$> half genInstantVector <*> liftGen (half genScalar),
      ConstantVector <$> half genScalar,
      ClampBelow <$> half genInstantVector <*> half genScalar,
      ClampAbove <$> half genInstantVector <*> half genScalar,
      Clamp <$> half genInstantVector <*> half genScalar <*> half genScalar,
      AbsentOverTime <$> half genRange,
      HistogramQuantile <$> half genScalar <*> half genInstantVector
    ]
  where
    overAVector constructor = constructor <$> half genInstantVector

genAggregation :: Gen Aggregation
genAggregation =
  oneof
    [ pure Sum,
      pure Average,
      pure Minimum,
      pure Maximum,
      pure Count,
      CountValues <$> genValid,
      pure Group,
      pure StandardDeviation,
      pure StandardVariance,
      TopK <$> half genScalar,
      BottomK <$> half genScalar,
      Quantile <$> half genScalar
    ]

-- | Only the operators that take a number on either side.  The set operators
-- are not here: they take a vector on both.
genArithmetic :: Gen BinOp
genArithmetic =
  oneof
    [ elements [Add, Subtract, Multiply, Divide, Modulo, Power, Atan2],
      Compare <$> genValid <*> genValid
    ]

-- | The same, for the two sides of a comparison that are both scalars:
-- Prometheus refuses one of those without the bool modifier, because what it
-- would otherwise be asked to do is filter a number out of existence.
genScalarArithmetic :: Gen BinOp
genScalarArithmetic =
  oneof
    [ elements [Add, Subtract, Multiply, Divide, Modulo, Power, Atan2],
      (`Compare` AsZeroOrOne) <$> genValid
    ]

-- | A number a query can carry, which is any but a NaN or an infinity.
genQueryNumber :: Gen Double
genQueryNumber = genValid `suchThat` (\x -> not (isNaN x || isInfinite x))

half :: Gen a -> Gen a
half = scale (`div` 2)

liftGen :: Gen a -> Gen (Maybe a)
liftGen generator = oneof [pure Nothing, Just <$> generator]

instance GenValid Aggregation where
  genValid = genAggregation
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Grouping

instance GenValid BinOp

instance GenValid Comparison

instance GenValid Answering

instance GenValid Matching

instance GenValid Call where
  genValid = genCall
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid RangeFunction

instance GenValid RangeVector where
  genValid = genRange
  shrinkValid = filter isValid . shrinkValidStructurally

-- | One selector's samples over a window, or an expression sampled over one.
genRange :: Gen RangeVector
genRange =
  oneof
    [ Over <$> genValid <*> genValid,
      Sampled <$> half genInstantVector <*> genValid <*> genValid
    ]

instance GenValid Replacement where
  genValid =
    Replacement
      <$> genValid
      <*> genQueryText
      <*> genValid
      <*> genValid
  shrinkValid = filter isValid . shrinkValidStructurally

-- | A selector that narrows the series down to something, which is what
-- 'Validity' asks of one.
--
-- Half of them name a metric and half do not, because a selector with only
-- labels is how a log stream is picked out and is the half more likely to be
-- got wrong.
instance GenValid Selector where
  genValid = do
    matchers <- genValid
    narrowing <- genValid `suchThat` matcherNarrows
    offset <- genValid
    named <- genValid
    name <- genValid
    pure
      Selector
        { selectorMetric = if named then Just name else Nothing,
          -- The one that narrows goes in whether or not a metric is named, so
          -- that a named selector's matchers are not all negative either.
          selectorMatchers = narrowing : matchers,
          selectorOffset = offset
        }
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Matcher

-- | A value a matcher can carry, which is text Prometheus can read back.
--
-- Arbitrary Text can hold a lone surrogate, which is not a character any
-- UTF-8 encoding of the query can carry, and the server answers "invalid
-- UTF-8 rune" rather than anything about the query.
genQueryText :: Gen Text
genQueryText = Text.pack <$> genListOf (genValid `suchThat` isSafeCharacter)
  where
    isSafeCharacter character =
      character /= '\\' && not (isSurrogate character)
    isSurrogate character =
      let point = fromEnum character
       in point >= 0xD800 && point <= 0xDFFF

instance GenValid Match where
  genValid =
    oneof
      [ Is <$> genQueryText,
        IsNot <$> genQueryText,
        Matches <$> genValid,
        DoesNotMatch <$> genValid
      ]
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Regexp where
  genValid =
    oneof
      [ Literally <$> genQueryText,
        AnyOf <$> genNonEmptyOf genQueryText,
        StartingWith <$> genQueryText,
        -- A pattern is a regular expression somebody wrote, so the only ones
        -- generated here are ones that are certainly regular expressions.
        Pattern <$> genIdentifier
      ]
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Duration where
  genValid =
    oneof
      [ Milliseconds <$> chooseWord 9223372036854,
        Seconds <$> chooseWord 9223372036,
        Minutes <$> chooseWord 153722867,
        Hours <$> chooseWord 2562047,
        Days <$> chooseWord 106751,
        Weeks <$> chooseWord 15250,
        Years <$> chooseWord 292
        -- No DurationVariable: what one stands for is not PromQL's business,
        -- so a query carrying one is not a query until whatever put it there
        -- has substituted it.  'genDurationVariable' makes those.
      ]
    where
      chooseWord upper = chooseBoundedIntegral (0, upper)
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid LabelName where
  genValid = LabelName <$> genIdentifier
  shrinkValid = filter isValid . map LabelName . shrinkValid . unLabelName

instance GenValid MetricName where
  genValid = MetricName <$> genIdentifier
  shrinkValid = filter isValid . map MetricName . shrinkValid . unMetricName

-- | A window written as something else to fill in later, such as one of
-- Grafana's interval variables.
genDurationVariable :: Gen Duration
genDurationVariable = DurationVariable <$> genIdentifier

-- | A name Prometheus will read as one.
--
-- Built out of the characters a name may have rather than filtered out of
-- arbitrary text, because the text that happens to be an identifier is a
-- vanishing fraction of the text a filter would be handed.
genIdentifier :: Gen Text
genIdentifier = do
  firstCharacter <- elements startCharacters
  rest <- genListOf (elements (startCharacters ++ ['0' .. '9']))
  pure (Text.pack (firstCharacter : rest))
  where
    startCharacters = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
