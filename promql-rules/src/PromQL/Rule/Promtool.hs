{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Asking promtool about rules, from a test suite.
--
-- promtool comes with Prometheus and evaluates rules with Prometheus's own
-- engine, so whether a rule file loads and whether an alert fires when it
-- should are both answered by the thing that will be running them.
--
-- A sydtest suite asserts with @rulesShouldPass@ and @rulesShouldLoad@ from
-- promql-rules-gen's "PromQL.Rule.Sydtest", which print what promtool
-- printed as it printed it.
-- Any other suite asserts on the 'PromtoolResult' or the 'RuleTestResult'.
module PromQL.Rule.Promtool
  ( PromtoolResult (..),
    checkRules,
    RuleTestResult (..),
    testRules,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import PromQL.Rule
import PromQL.Rule.Test
import System.Exit (ExitCode (..))
import System.Process.Typed (proc, readProcess, setWorkingDir)

-- | What promtool made of the rules.
data PromtoolResult
  = PromtoolPassed
  | -- | Everything promtool printed, which says what failed and how.
    PromtoolFailed !String
  deriving (Show, Eq)

-- | Whether Prometheus would load these rules: whether each expression
-- parses, and each template, and each label.
checkRules :: RuleFile -> IO PromtoolResult
checkRules ruleFile =
  withSystemTempDir "promql-rules" $ \directory -> do
    ByteString.writeFile (toFilePath (directory </> ruleFileName)) (renderRuleFile ruleFile)
    runPromtool directory ["check", "rules", toFilePath ruleFileName]

-- | What came of testing the rules.
data RuleTestResult
  = RuleTestsPassed
  | -- | Everything promtool printed, which says which tests failed and how.
    RuleTestsFailed !String
  | -- | The tests were not run, because they test these alerts and no rule
    -- in the file defines them.
    AlertsWithoutRules !(NonEmpty AlertName)
  deriving (Show, Eq)

-- | Whether the rules answer each test the way it says they should.
testRules :: RuleFile -> RuleTestFile -> IO RuleTestResult
testRules ruleFile testFile =
  case nonEmpty (alertsWithoutRules ruleFile testFile) of
    Just missing -> pure (AlertsWithoutRules missing)
    Nothing ->
      withSystemTempDir "promql-rules" $ \directory -> do
        ByteString.writeFile (toFilePath (directory </> ruleFileName)) (renderRuleFile ruleFile)
        ByteString.writeFile
          (toFilePath (directory </> testFileName))
          (renderRuleTestFile [Rel ruleFileName] testFile)
        result <- runPromtool directory ["test", "rules", toFilePath testFileName]
        pure $ case result of
          PromtoolPassed -> RuleTestsPassed
          PromtoolFailed output -> RuleTestsFailed output

-- | Relative, so that what promtool prints names the file and not wherever
-- the temporary directory happened to be.
ruleFileName :: Path Rel File
ruleFileName = [relfile|rules.json|]

testFileName :: Path Rel File
testFileName = [relfile|tests.json|]

-- | A promtool that cannot be run throws rather than coming back as a
-- failure: a test told that its rules failed would go looking at the rules,
-- and the thing to look at would be the test's environment.
--
-- What promtool prints is read as bytes and decoded as UTF-8, which is what
-- promtool writes whatever the locale, so that a name that is not ASCII
-- comes back as itself rather than as an error about decoding it.
runPromtool :: Path Abs Dir -> [String] -> IO PromtoolResult
runPromtool directory arguments = do
  attempt <-
    try $
      readProcess (setWorkingDir (toFilePath directory) (proc "promtool" arguments))
  case attempt of
    Left (exception :: IOException) ->
      ioError . userError $
        unlines
          [ unwords ["Could not run promtool:", show exception],
            "promtool comes with Prometheus, and has to be on the PATH of whatever runs these tests."
          ]
    Right (ExitSuccess, _, _) -> pure PromtoolPassed
    Right (ExitFailure _, out, err) ->
      pure . PromtoolFailed . Text.unpack . Text.decodeUtf8With lenientDecode $
        LazyByteString.toStrict (LazyByteString.concat [out, err])
