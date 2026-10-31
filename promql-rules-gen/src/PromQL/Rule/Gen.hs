{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators for the rule and rule-test types.
--
-- The rules generated here are ones Prometheus will load, which is more than
-- 'Data.Validity.Validity' asks of them: label values and annotations are
-- templates, and text that happens to open one is a template that does not
-- parse.
module PromQL.Rule.Gen
  ( genSegments,
  )
where

import Data.Char (isPrint)
import Data.Function (on)
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.List (nubBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import PromQL
import PromQL.Gen ()
import PromQL.Rule
import PromQL.Rule.Test
import Test.QuickCheck

instance GenValid RuleFile where
  genValid = RuleFile . nubBy ((==) `on` ruleGroupName) <$> genValid
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid RuleGroup where
  genValid =
    RuleGroup
      <$> genValid
      <*> genValid
      -- A group's rules are evaluated one after the other, and a few are
      -- enough to say that each is written out.
      <*> scale (`div` 4) (genListOf genValid)
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid RuleGroupName where
  genValid = RuleGroupName <$> genNonEmptyText
  shrinkValid = filter isValid . map RuleGroupName . shrinkValid . unRuleGroupName

instance GenValid Rule where
  genValid = oneof [Alerting <$> genValid, Recording <$> genValid]
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid AlertingRule where
  genValid =
    AlertingRule
      <$> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genRuleLabels
      <*> genRuleLabels
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid AlertName where
  genValid = AlertName <$> genNonEmptyText
  shrinkValid = filter isValid . map AlertName . shrinkValid . unAlertName

instance GenValid RecordingRule where
  genValid =
    RecordingRule
      <$> genValid
      <*> genValid
      <*> genRuleLabels
  shrinkValid = filter isValid . shrinkValidStructurally

-- | Labels and annotations for a rule, whose values are templates and so are
-- generated without anything that would open one.
genRuleLabels :: Gen (Map LabelName Text)
genRuleLabels =
  Map.fromList
    <$> genListOf
      ( (,)
          <$> (genValid `suchThat` (/= LabelName "__name__"))
          <*> genTemplateFreeText
      )

-- | Text that Prometheus reads as itself where it reads a template, which is
-- text without an opening brace in it.
--
-- Printable, too, because that is what a label or an annotation is for, and
-- without U+FFFD, because a series' labels are written into a query (see
-- 'PromQL.validateQueryText').
genTemplateFreeText :: Gen Text
genTemplateFreeText =
  Text.pack
    <$> genListOf
      (genValid `suchThat` (\character -> isPrint character && character /= '{' && character /= '\xFFFD'))

genNonEmptyText :: Gen Text
genNonEmptyText = genTemplateFreeText `suchThat` (not . Text.null)

instance GenValid RuleTestFile where
  genValid = RuleTestFile <$> genValid <*> scale (`div` 4) (genListOf genValid)
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid RuleTest where
  genValid =
    RuleTest
      <$> genTemplateFreeText
      <*> genValid
      <*> genRuleLabels
      <*> (nubBy ((==) `on` inputSeriesSeries) <$> genValid)
      <*> genValid
      <*> genValid
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid InputSeries where
  genValid =
    InputSeries
      <$> (genValid `suchThat` \series' -> isJust (seriesMetric series') || not (Map.null (seriesLabels series')))
      <*> genListOf genSegment
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Series where
  genValid =
    Series
      <$> genValid
      <*> ( Map.fromList
              <$> genListOf
                ( (,)
                    <$> (genValid `suchThat` (/= LabelName "__name__"))
                    <*> genNonEmptyText
                )
          )
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid Segment where
  genValid = genSegment
  shrinkValid = filter isValid . shrinkValidStructurally

-- | A segment of a length a test would write, with numbers small enough that
-- a ramp of them stays finite.
genSegment :: Gen Segment
genSegment =
  frequency
    [ (4, Samples <$> genValid),
      (2, Missing <$> chooseBoundedIntegral (1, 8)),
      (1, pure Stale)
    ]

instance GenValid Ramp where
  genValid =
    Ramp
      <$> genSampleValue
      <*> oneof [pure 0, genSampleValue]
      <*> chooseBoundedIntegral (1, 8)
  shrinkValid = filter isValid . shrinkValidStructurally

-- | Whole numbers as well as fractions, because the two are written
-- differently and both have to come out as the number that was meant.
genSampleValue :: Gen Double
genSampleValue =
  oneof
    [ fromInteger <$> choose (-1000, 1000),
      choose (-1000, 1000),
      choose (-1, 1)
    ]

instance GenValid AlertTest where
  genValid =
    AlertTest
      <$> genValid
      <*> genValid
      <*> (nubBy ((==) `on` expectedAlertLabels) <$> genValid)
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid ExpectedAlert

instance GenValid ExprTest where
  genValid =
    ExprTest
      <$> genValid
      <*> genValid
      <*> (nubBy ((==) `on` expectedSampleSeries) <$> genValid)
  shrinkValid = filter isValid . shrinkValidStructurally

instance GenValid ExpectedSample where
  genValid = ExpectedSample <$> genValid <*> genSampleValue
  shrinkValid = filter isValid . shrinkValidStructurally

-- | The steps of a series, at least one of them, because a series with none
-- is one promtool has nothing to read.
genSegments :: Gen [Segment]
genSegments = (:) <$> genSegment <*> genListOf genSegment
