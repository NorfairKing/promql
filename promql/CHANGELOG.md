# Changelog

## [Unreleased]

### Added

* The first version: an expression type for PromQL, and a renderer that is
  the only thing that writes any.
* A parser, `PromQL.Parse`, that reads back what the renderer writes.
* `HasCodec` instances for expressions, durations and names, in `PromQL.Codec`.
