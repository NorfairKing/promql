{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Prometheus's alerting and recording rules, and the file it reads them
-- from.
--
-- A rule's expression is an 'Expr' rather than text, for the same reasons a
-- query is.
-- What a rule adds to its expression is a name, a few durations and some
-- labels, and those are where a rule file written by hand goes wrong in ways
-- Prometheus only reports when it loads the file.
--
-- The codecs and the renderer are left out of mutation testing: the golden
-- files of what they write fail on any change to it, and the roundtrip
-- properties on any change to what they read.
module PromQL.Rule
  ( RuleFile (..),
    RuleGroup (..),
    RuleGroupName (..),
    Rule (..),
    AlertingRule (..),
    AlertName (..),
    RecordingRule (..),
    renderRuleFile,
    validateFixedDuration,
  )
where

import Autodocodec
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)
import PromQL
import PromQL.Codec ()
import PromQL.Rule.Fields

-- | What one file of rules holds.
data RuleFile = RuleFile
  { ruleFileGroups :: ![RuleGroup]
  }
  deriving (Show, Eq, Generic)

-- | Prometheus refuses a file in which two groups have the same name.
instance Validity RuleFile where
  validate ruleFile =
    mconcat
      [ genericValidate ruleFile,
        declare "no two groups have the same name" $
          let names = map ruleGroupName (ruleFileGroups ruleFile)
           in nub names == names
      ]

instance HasCodec RuleFile where
  codec = ruleFileCodec

{-# ANN ruleFileCodec ("DisableMutations" :: String) #-}
ruleFileCodec :: JSONCodec RuleFile
ruleFileCodec =
  validatedObject "RuleFile" $
    RuleFile
      <$> requiredField "groups" "The groups of rules" .= ruleFileGroups

-- | Rules that are evaluated together, one after the other, at the same
-- interval.
data RuleGroup = RuleGroup
  { ruleGroupName :: !RuleGroupName,
    -- | How often the group is evaluated, where it is not the global
    -- evaluation interval.
    ruleGroupInterval :: !(Maybe Duration),
    ruleGroupRules :: ![Rule]
  }
  deriving (Show, Eq, Generic)

instance Validity RuleGroup where
  validate ruleGroup =
    mconcat
      [ genericValidate ruleGroup,
        foldMap validateFixedDuration (ruleGroupInterval ruleGroup)
      ]

instance HasCodec RuleGroup where
  codec = ruleGroupCodec

{-# ANN ruleGroupCodec ("DisableMutations" :: String) #-}
ruleGroupCodec :: JSONCodec RuleGroup
ruleGroupCodec =
  validatedObject "RuleGroup" $
    RuleGroup
      <$> requiredField "name" "What the group is called" .= ruleGroupName
      <*> optionalField "interval" "How often the group is evaluated" .= ruleGroupInterval
      <*> optionalFieldWithDefault "rules" [] "The rules, evaluated in order" .= ruleGroupRules

newtype RuleGroupName = RuleGroupName {unRuleGroupName :: Text}
  deriving (Show, Eq, Ord, Generic)

-- | Without 'genericValidate', here and for 'AlertName': a 'Text' is always
-- valid, so it would add nothing, and the mutation testing says as much.
instance Validity RuleGroupName where
  validate name =
    declare "the group name is not empty" $
      not (Text.null (unRuleGroupName name))

instance HasCodec RuleGroupName where
  codec = ruleGroupNameCodec

ruleGroupNameCodec :: JSONCodec RuleGroupName
ruleGroupNameCodec = validatedText RuleGroupName unRuleGroupName

data Rule
  = Alerting !AlertingRule
  | Recording !RecordingRule
  deriving (Show, Eq, Generic)

instance Validity Rule

-- | Told apart by which of @alert@ and @record@ a rule has.
instance HasCodec Rule where
  codec = ruleCodec

ruleCodec :: JSONCodec Rule
ruleCodec =
  dimapCodec
    (either Alerting Recording)
    ( \case
        Alerting rule -> Left rule
        Recording rule -> Right rule
    )
    (disjointEitherCodec (codec @AlertingRule) (codec @RecordingRule))

-- | A rule that fires an alert for each series its expression answers with.
--
-- Label values and annotations are Go templates, which Prometheus expands
-- with the alert's labels and value.
-- Nothing here reads a template, so one that does not parse is what
-- 'PromQL.Rule.Promtool.checkRules' is for.
data AlertingRule = AlertingRule
  { alertingRuleName :: !AlertName,
    alertingRuleExpr :: !Expr,
    -- | How long the expression has to keep answering before the alert fires
    -- rather than only being pending.
    alertingRuleFor :: !(Maybe Duration),
    -- | How long the alert keeps firing after the expression stops answering.
    alertingRuleKeepFiringFor :: !(Maybe Duration),
    alertingRuleLabels :: !(Map LabelName Text),
    alertingRuleAnnotations :: !(Map LabelName Text)
  }
  deriving (Show, Eq, Generic)

instance Validity AlertingRule where
  validate rule =
    mconcat
      [ genericValidate rule,
        foldMap validateFixedDuration (alertingRuleFor rule),
        foldMap validateFixedDuration (alertingRuleKeepFiringFor rule),
        validateRuleLabels (alertingRuleLabels rule)
      ]

instance HasCodec AlertingRule where
  codec = alertingRuleCodec

{-# ANN alertingRuleCodec ("DisableMutations" :: String) #-}
alertingRuleCodec :: JSONCodec AlertingRule
alertingRuleCodec =
  validatedObject "AlertingRule" $
    AlertingRule
      <$> requiredField "alert" "The alert's name" .= alertingRuleName
      <*> requiredField "expr" "The expression whose answers are the alerts" .= alertingRuleExpr
      <*> optionalField "for" "How long the expression answers before the alert fires" .= alertingRuleFor
      <*> optionalField "keep_firing_for" "How long the alert keeps firing after that" .= alertingRuleKeepFiringFor
      <*> labelMapField "labels" "The labels every alert carries" .= alertingRuleLabels
      <*> labelMapField "annotations" "The annotations every alert carries" .= alertingRuleAnnotations

-- | The name an alert goes by, which is the value of its @alertname@ label.
--
-- Not a 'LabelName': it is a label's value, so it may be any text, spaces
-- included.
newtype AlertName = AlertName {unAlertName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity AlertName where
  validate name =
    declare "the alert name is not empty" $
      not (Text.null (unAlertName name))

instance HasCodec AlertName where
  codec = alertNameCodec

alertNameCodec :: JSONCodec AlertName
alertNameCodec = validatedText AlertName unAlertName

-- | A rule that writes its expression's answer back as a series of its own.
data RecordingRule = RecordingRule
  { -- | The metric the answer is written as, which is where the colons a
    -- metric name may carry are meant to go.
    recordingRuleName :: !MetricName,
    recordingRuleExpr :: !Expr,
    recordingRuleLabels :: !(Map LabelName Text)
  }
  deriving (Show, Eq, Generic)

instance Validity RecordingRule where
  validate rule =
    mconcat
      [ genericValidate rule,
        validateRuleLabels (recordingRuleLabels rule)
      ]

instance HasCodec RecordingRule where
  codec = recordingRuleCodec

{-# ANN recordingRuleCodec ("DisableMutations" :: String) #-}
recordingRuleCodec :: JSONCodec RecordingRule
recordingRuleCodec =
  validatedObject "RecordingRule" $
    RecordingRule
      <$> requiredField "record" "The metric the answer is written as" .= recordingRuleName
      <*> requiredField "expr" "The expression whose answer is recorded" .= recordingRuleExpr
      <*> labelMapField "labels" "The labels the answer is written with" .= recordingRuleLabels

-- | A duration that Prometheus reads out of a configuration file rather than
-- out of a query, where there is nothing that would fill in a variable.
validateFixedDuration :: Duration -> Validation
validateFixedDuration duration =
  declare "the duration is not a variable" $ case duration of
    DurationVariable _ -> False
    Milliseconds _ -> True
    Seconds _ -> True
    Minutes _ -> True
    Hours _ -> True
    Days _ -> True
    Weeks _ -> True
    Years _ -> True

-- | The labels a rule adds to what it answers with.
--
-- Prometheus refuses a rule that sets the metric name this way: a recording
-- rule's name is the place for that, and an alert has none.
validateRuleLabels :: Map LabelName Text -> Validation
validateRuleLabels labels =
  declare "the labels do not set the metric name" $
    not (Map.member (LabelName "__name__") labels)

-- | A file of rules as Prometheus reads one.
renderRuleFile :: RuleFile -> ByteString
renderRuleFile = renderFile (codec @RuleFile)
