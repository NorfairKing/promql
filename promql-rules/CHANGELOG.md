# Changelog

## [Unreleased]

### Added

* The first version: alerting and recording rules, the file Prometheus reads
  them from, unit tests for them in the format `promtool test rules` reads,
  and a way to run those tests from a test suite.
* `HasCodec` instances for every rule and rule-test type. Files are written
  as JSON.
