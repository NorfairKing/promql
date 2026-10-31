{-# LANGUAGE LambdaCase #-}

-- | Rule tests as sydtest assertions.
--
-- A failure shows what promtool printed as it printed it, rather than as the
-- escaped string inside a 'RuleTestsFailed' that a 'shouldBe' would show.
--
-- > it "pages when an instance goes down" $
-- >   rulesShouldPass rules tests
module PromQL.Rule.Sydtest
  ( rulesShouldLoad,
    rulesShouldPass,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PromQL.Rule
import PromQL.Rule.Promtool
import PromQL.Rule.Test
import Test.Syd

-- | Assert that Prometheus would load these rules.
rulesShouldLoad :: RuleFile -> IO ()
rulesShouldLoad ruleFile =
  checkRules ruleFile >>= \case
    PromtoolPassed -> pure ()
    PromtoolFailed output -> expectationFailure output

-- | Assert that the rules answer each test the way it says they should.
rulesShouldPass :: RuleFile -> RuleTestFile -> IO ()
rulesShouldPass ruleFile testFile =
  testRules ruleFile testFile >>= \case
    RuleTestsPassed -> pure ()
    RuleTestsFailed output -> expectationFailure output
    AlertsWithoutRules names ->
      expectationFailure . unlines $
        "These alerts are tested, but no rule in the file defines them, so a test of them would pass whatever the rules said:"
          : map (Text.unpack . unAlertName) (NonEmpty.toList names)
