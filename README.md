# promql

PromQL as a type rather than as text.

A query assembled by concatenating strings carries the grammar in its author's
head.  Three things go wrong that way, and all three are silent:

* the quoting of a label's value;
* the backslashes in a matcher's regular expression, which are read twice
  over, once as a string and once as a regex;
* the parentheses around a division, which PromQL will accept either way and
  answer differently.

None of the three is expressible here.  `PromQL.Render.render` is the only
thing in the library that writes any PromQL, so it is the only thing that has
to be right about the grammar, and it is the only thing a test has to look at.

```haskell
render $
  Binary Divide OnEverything
    (Aggregate Sum Nothing
      (OverTime Rate (Over (metric "http_requests_total") (Minutes 5))))
    (Apply ClampBelow
      [ Aggregate Sum Nothing
          (OverTime Rate (Over (metric "http_requests_total") (Minutes 5)))
      , Number 1e-9
      ])
```

## What it covers

The grammar a dashboard or an alerting rule asks for: selectors with their
matchers and offsets, range vectors and subqueries, the aggregations with
their `by` and `without` clauses, the binary operators with their precedence
and their vector matching, and a named set of functions.

It is deliberately not every PromQL function.  A function is a constructor and
a line in the renderer, which is the point: a query cannot name one that does
not exist.  Adding the one you need is a two-line change.

There is no parser.  Nothing here reads PromQL, only writes it.

## How it is checked

By a real Prometheus, in `promql-e2e`.  Prometheus's own parser is the only
thing that can say whether a query is valid PromQL, so the end-to-end check
starts one and asks it:

* every expression the generators can write parses;
* leaving out the brackets the grammar does not need means the same as putting
  every one of them in, which is how the precedence table is checked against
  the one Prometheus implements;
* breaking a query across lines means the same as writing it on one.

`promtool promql format` cannot answer the second: it keeps whatever brackets
it was given.  The `/api/v1/parse_query` endpoint hands back the tree.
