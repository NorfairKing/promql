{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Writing an expression out as the text Prometheus reads.
--
-- The one place in this library that has to be right about the grammar, which
-- is why it is the one place that writes any.  The parentheses are its to put
-- in: an expression tree says what it means, and the text has to read back as
-- the same tree.
module PromQL.Render
  ( render,
    renderFlat,
    renderBracketed,
    renderSelector,
    renderMatcher,
    renderRegexp,
    renderDuration,
    renderNumber,
    queryWidth,
    precedence,
    operatorPrecedence,
    lowestPrecedence,
    atomPrecedence,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import PromQL.Expr

-- | An expression as PromQL reads it, over as many lines as it takes.
render :: Expr -> Text
render = layOut 0 lowestPrecedence

-- | An expression on one line, however long that line comes out.
renderFlat :: Expr -> Text
renderFlat = renderIn lowestPrecedence

-- | An expression with brackets around every part of it that can take them,
-- whether the grammar needs them or not.
--
-- Not for writing a query anybody has to read: this is the oracle the
-- precedence table is checked against.  A parser given this and given
-- 'render' has to answer with the same tree, and where it does not, the
-- precedence written here disagrees with the one Prometheus implements.
renderBracketed :: Expr -> Text
renderBracketed expr = case shapeOf expr of
  Opaque text -> text
  Operator operator matchOn left right ->
    Text.concat
      [ "(",
        renderBracketed left,
        " ",
        renderBinOp operator,
        renderMatching matchOn,
        " ",
        renderBracketed right,
        ")"
      ]
  Applied before arguments after ->
    Text.concat
      [ before,
        Text.intercalate ", " (map bracketedArgument arguments),
        after
      ]
  where
    bracketedArgument = \case
      Given text -> text
      Nested inner -> renderBracketed inner

-- | How wide a query may get before it starts breaking across lines.
--
-- Queries are read in places that do not wrap them anywhere useful: a panel
-- editor, an alerting rule, a terminal.
queryWidth :: Int
queryWidth = 110

-- | An expression starting at a column, broken across lines only where it
-- does not fit.
--
-- Breaking from the outside in: an expression that fits is left alone, and
-- one that does not gives way at its own operator before any of its parts are
-- asked to.  That puts the break at the loosest operator, which is the one
-- that says what the query is doing.
layOut :: Int -> Word -> Expr -> Text
layOut column context expr
  | column + Text.length flat <= queryWidth = flat
  | otherwise = case shapeOf expr of
      -- Nothing to give way at: a selector is as long as its labels make it.
      Opaque text -> text
      Operator operator matchOn left right ->
        let joint = Text.concat [renderBinOp operator, renderMatching matchOn]
         in Text.concat
              [ opening,
                layOut inner (precedence expr) left,
                newlineAt (inner + 2),
                joint,
                " ",
                layOut (inner + 2 + Text.length joint + 1) (precedence expr + 1) right,
                closing
              ]
      -- Brackets are not this branch's business: what is written around a
      -- comma-separated list already has its own, and binds as tightly as a
      -- name does.
      Applied before arguments after ->
        Text.concat
          [ before,
            newlineAt (column + 2),
            Text.intercalate
              (Text.concat [",", newlineAt (column + 2)])
              (map (layOutArgument (column + 2)) arguments),
            newlineAt column,
            after
          ]
  where
    flat = renderIn context expr
    bracketed = precedence expr < context
    opening = if bracketed then "(" else ""
    closing = if bracketed then ")" else ""
    inner = column + Text.length opening
    newlineAt at = Text.concat ["\n", Text.replicate at " "]

layOutArgument :: Int -> Argument -> Text
layOutArgument column = \case
  Given text -> text
  Nested expr -> layOut column lowestPrecedence expr

-- | What an expression can be broken at, which is the only thing the layout
-- needs to know about one.
data Shape
  = -- | Text with nowhere to break in it.
    Opaque !Text
  | Operator !BinOp !Matching !Expr !Expr
  | -- | Something written around a comma-separated list: what comes before
    -- the arguments, the arguments, and what comes after them.
    Applied !Text ![Argument] !Text

data Argument
  = Given !Text
  | Nested !Expr

shapeOf :: Expr -> Shape
shapeOf = \case
  Number x -> Opaque (renderNumber x)
  Window window -> Opaque (renderDuration window)
  String text -> Opaque (quoted text)
  Vector selector -> Opaque (renderSelector selector)
  OverTime function rangeVector ->
    appliedTo (renderRangeFunction function) [Given (renderRangeVector rangeVector)]
  QuantileOverTime quantile rangeVector ->
    appliedTo "quantile_over_time" [Nested quantile, Given (renderRangeVector rangeVector)]
  Apply called -> let (name, arguments) = callParts called in appliedTo name arguments
  LabelReplace inner replacement ->
    appliedTo
      "label_replace"
      [ Nested inner,
        Given (quoted (unLabelName (replacementDestination replacement))),
        Given (quoted (replacementText replacement)),
        Given (quoted (unLabelName (replacementSource replacement))),
        Given (quoted (renderRegexp (replacementRegexp replacement)))
      ]
  Aggregate aggregation grouping inner ->
    Applied
      ( Text.concat
          [ renderAggregation aggregation,
            maybe "" (\keeping -> Text.concat [" ", renderGrouping keeping]) grouping,
            " ("
          ]
      )
      (aggregationArguments aggregation ++ [Nested inner])
      ")"
  Binary operator matchOn left right -> Operator operator matchOn left right
  where
    appliedTo before arguments = Applied (Text.concat [before, "("]) arguments ")"

-- | An expression on one line, parenthesised where it would otherwise bind
-- more loosely than its surroundings.
renderIn :: Word -> Expr -> Text
renderIn context expr =
  let rendered = renderExpr expr
   in if precedence expr < context
        then Text.concat ["(", rendered, ")"]
        else rendered

-- | An expression on one line, whatever its length: what the layout measures
-- to decide whether it needs more than one.
renderExpr :: Expr -> Text
renderExpr = \case
  Number x -> renderNumber x
  Window window -> renderDuration window
  String text -> quoted text
  Vector selector -> renderSelector selector
  OverTime function rangeVector ->
    call (renderRangeFunction function) [renderRangeVector rangeVector]
  QuantileOverTime quantile rangeVector ->
    call "quantile_over_time" [renderFlat quantile, renderRangeVector rangeVector]
  Apply called -> let (name, arguments) = callParts called in call name (map flatArgument arguments)
  LabelReplace inner replacement ->
    call
      "label_replace"
      [ renderFlat inner,
        quoted (unLabelName (replacementDestination replacement)),
        quoted (replacementText replacement),
        quoted (unLabelName (replacementSource replacement)),
        quoted (renderRegexp (replacementRegexp replacement))
      ]
  Aggregate aggregation grouping inner ->
    Text.concat
      [ renderAggregation aggregation,
        maybe "" (\keeping -> Text.concat [" ", renderGrouping keeping]) grouping,
        " (",
        Text.intercalate ", " (map flatArgument (aggregationArguments aggregation) ++ [renderFlat inner]),
        ")"
      ]
  -- Both sides are rendered as though they bound one step more tightly than
  -- this operator, which puts parentheses around a child that does not, and
  -- leaves them off a child that does.  Every PromQL operator but
  -- exponentiation associates to the left, so only the right-hand side needs
  -- the extra step.
  Binary operator matchOn left right ->
    Text.unwords
      [ renderIn (sideContext operator LeftSide) left,
        Text.concat [renderBinOp operator, renderMatching matchOn],
        renderIn (sideContext operator RightSide) right
      ]

data Side
  = LeftSide
  | RightSide

-- | What an operand has to bind at least as tightly as to go without
-- brackets, which depends on which side of the operator it is on.
sideContext :: BinOp -> Side -> Word
sideContext operator side = case (operator, side) of
  -- Exponentiation is the one right-associative operator, so it is the left
  -- operand that needs the extra step.
  (Power, LeftSide) -> operatorPrecedence Power + 1
  (Power, RightSide) -> operatorPrecedence Power
  (_, LeftSide) -> operatorPrecedence operator
  (_, RightSide) -> operatorPrecedence operator + 1

flatArgument :: Argument -> Text
flatArgument = \case
  Given text -> text
  Nested expr -> renderFlat expr

-- | The arguments an aggregation takes besides the vector itself.
aggregationArguments :: Aggregation -> [Argument]
aggregationArguments = \case
  CountValues label -> [Given (quoted (unLabelName label))]
  TopK k -> [Nested k]
  BottomK k -> [Nested k]
  Quantile quantile -> [Nested quantile]
  Sum -> []
  Average -> []
  Minimum -> []
  Maximum -> []
  Count -> []
  Group -> []
  StandardDeviation -> []
  StandardVariance -> []

call :: Text -> [Text] -> Text
call function arguments = Text.concat [function, "(", Text.intercalate ", " arguments, ")"]

quoted :: Text -> Text
quoted value = Text.concat ["\"", escaped value, "\""]

-- | A string as PromQL reads one, which is to say with its backslashes and
-- its quotes written as escapes.
escaped :: Text -> Text
escaped = Text.concatMap $ \case
  '\\' -> "\\\\"
  '"' -> "\\\""
  '\n' -> "\\n"
  character -> Text.singleton character

-- | How tightly an expression holds on to its neighbours, following
-- Prometheus's own table.  A larger number binds more tightly.
precedence :: Expr -> Word
precedence = \case
  Binary operator _ _ _ -> operatorPrecedence operator
  -- Everything else is a name, a number, or something with its own brackets
  -- around it, and nothing can come between its parts.
  Number _ -> atomPrecedence
  Window _ -> atomPrecedence
  String _ -> atomPrecedence
  Vector _ -> atomPrecedence
  OverTime _ _ -> atomPrecedence
  QuantileOverTime _ _ -> atomPrecedence
  Apply _ -> atomPrecedence
  LabelReplace _ _ -> atomPrecedence
  Aggregate {} -> atomPrecedence

operatorPrecedence :: BinOp -> Word
operatorPrecedence = \case
  Or -> 1
  And -> 2
  Unless -> 2
  Compare _ _ -> 3
  Add -> 4
  Subtract -> 4
  Multiply -> 5
  Divide -> 5
  Modulo -> 5
  Atan2 -> 5
  Power -> 6

-- | Loose enough that nothing is parenthesised for being at the top.
lowestPrecedence :: Word
lowestPrecedence = 0

atomPrecedence :: Word
atomPrecedence = 7

renderAggregation :: Aggregation -> Text
renderAggregation = \case
  Sum -> "sum"
  Average -> "avg"
  Minimum -> "min"
  Maximum -> "max"
  Count -> "count"
  CountValues _ -> "count_values"
  Group -> "group"
  StandardDeviation -> "stddev"
  StandardVariance -> "stdvar"
  TopK _ -> "topk"
  BottomK _ -> "bottomk"
  Quantile _ -> "quantile"

renderGrouping :: Grouping -> Text
renderGrouping = \case
  By labels -> labelled "by" labels
  Without labels -> labelled "without" labels
  where
    labelled keyword labels =
      Text.concat [keyword, " (", Text.intercalate ", " (map unLabelName (NE.toList labels)), ")"]

-- | What a call is written as: the function's name, and the arguments it was
-- given, in order.
callParts :: Call -> (Text, [Argument])
callParts = \case
  Absolute inner -> ("abs", [Nested inner])
  Ceiling inner -> ("ceil", [Nested inner])
  Floor inner -> ("floor", [Nested inner])
  Exponential inner -> ("exp", [Nested inner])
  NaturalLog inner -> ("ln", [Nested inner])
  Log2 inner -> ("log2", [Nested inner])
  Log10 inner -> ("log10", [Nested inner])
  SquareRoot inner -> ("sqrt", [Nested inner])
  Sign inner -> ("sgn", [Nested inner])
  Round inner places -> ("round", Nested inner : map Nested (maybe [] pure places))
  Scalar inner -> ("scalar", [Nested inner])
  ConstantVector inner -> ("vector", [Nested inner])
  ClampBelow inner lower -> ("clamp_min", [Nested inner, Nested lower])
  ClampAbove inner upper -> ("clamp_max", [Nested inner, Nested upper])
  Clamp inner lower upper -> ("clamp", [Nested inner, Nested lower, Nested upper])
  Absent inner -> ("absent", [Nested inner])
  AbsentOverTime rangeVector -> ("absent_over_time", [Given (renderRangeVector rangeVector)])
  Timestamp inner -> ("timestamp", [Nested inner])
  HistogramQuantile quantile buckets -> ("histogram_quantile", [Nested quantile, Nested buckets])
  Sort inner -> ("sort", [Nested inner])
  SortDescending inner -> ("sort_desc", [Nested inner])

renderBinOp :: BinOp -> Text
renderBinOp = \case
  Add -> "+"
  Subtract -> "-"
  Multiply -> "*"
  Divide -> "/"
  Modulo -> "%"
  Power -> "^"
  Atan2 -> "atan2"
  Compare comparison answering ->
    Text.concat [renderComparison comparison, if answering then " bool" else ""]
  Or -> "or"
  And -> "and"
  Unless -> "unless"

renderComparison :: Comparison -> Text
renderComparison = \case
  Equal -> "=="
  NotEqual -> "!="
  LessThan -> "<"
  LessOrEqual -> "<="
  GreaterThan -> ">"
  GreaterOrEqual -> ">="

renderMatching :: Matching -> Text
renderMatching = \case
  OnEverything -> ""
  On labels -> Text.concat [" on (", Text.intercalate ", " (map unLabelName labels), ")"]
  Ignoring labels -> Text.concat [" ignoring (", Text.intercalate ", " (map unLabelName labels), ")"]

renderRangeFunction :: RangeFunction -> Text
renderRangeFunction = \case
  Rate -> "rate"
  InstantRate -> "irate"
  Increase -> "increase"
  Delta -> "delta"
  InstantDelta -> "idelta"
  Derivative -> "deriv"
  Changes -> "changes"
  Resets -> "resets"
  AverageOverTime -> "avg_over_time"
  MinimumOverTime -> "min_over_time"
  MaximumOverTime -> "max_over_time"
  SumOverTime -> "sum_over_time"
  CountOverTime -> "count_over_time"
  LastOverTime -> "last_over_time"
  PresentOverTime -> "present_over_time"
  StandardDeviationOverTime -> "stddev_over_time"
  StandardVarianceOverTime -> "stdvar_over_time"

renderRangeVector :: RangeVector -> Text
renderRangeVector = \case
  -- The offset goes after the range, not before it: Prometheus refuses
  -- "foo offset 5m[1h]" and means "foo[1h] offset 5m".
  Over selector window ->
    Text.concat
      [ renderSelector selector {selectorOffset = Nothing},
        "[",
        renderDuration window,
        "]",
        maybe "" (\offset -> Text.concat [" offset ", renderDuration offset]) (selectorOffset selector)
      ]
  -- A subquery brackets whatever it samples, so the expression inside needs
  -- parentheses of its own unless it already brought some.
  Sampled inner window step ->
    Text.concat
      [ renderIn atomPrecedence inner,
        "[",
        renderDuration window,
        ":",
        maybe "" renderDuration step,
        "]"
      ]

renderSelector :: Selector -> Text
renderSelector selector =
  Text.concat
    [ maybe "" unMetricName (selectorMetric selector),
      case selectorMatchers selector of
        -- A metric on its own needs no braces.
        [] -> ""
        matchers ->
          Text.concat
            [ "{",
              Text.intercalate ", " (map renderMatcher matchers),
              "}"
            ],
      maybe "" (\offset -> Text.concat [" offset ", renderDuration offset]) (selectorOffset selector)
    ]

renderMatcher :: Matcher -> Text
renderMatcher matcher =
  Text.concat [unLabelName (matcherLabel matcher), operator, quoted value]
  where
    (operator, value) = case matcherMatch matcher of
      Is literal -> ("=", literal)
      IsNot literal -> ("!=", literal)
      Matches regexp -> ("=~", renderRegexp regexp)
      DoesNotMatch regexp -> ("!~", renderRegexp regexp)

-- | A regular expression as its own text, before the quoting that a matcher
-- then puts around it.
renderRegexp :: Regexp -> Text
renderRegexp = \case
  Literally literal -> escapeRegexp literal
  AnyOf literals -> Text.intercalate "|" (map escapeRegexp (NE.toList literals))
  StartingWith prefix -> Text.concat [escapeRegexp prefix, ".*"]
  Pattern regexp -> regexp

-- | A literal as a regular expression that matches exactly it.
escapeRegexp :: Text -> Text
escapeRegexp = Text.concatMap $ \character ->
  if character `elem` ("\\.+*?()[]{}|^$" :: String)
    then Text.pack ['\\', character]
    else Text.singleton character

renderDuration :: Duration -> Text
renderDuration = \case
  Milliseconds n -> inUnit n "ms"
  Seconds n -> inUnit n "s"
  Minutes n -> inUnit n "m"
  Hours n -> inUnit n "h"
  Days n -> inUnit n "d"
  Weeks n -> inUnit n "w"
  Years n -> inUnit n "y"
  DurationVariable variable -> variable
  where
    inUnit :: Word -> Text -> Text
    inUnit n unit = Text.concat [Text.pack (show @Word n), unit]

-- | A number as PromQL reads one.
--
-- Whole numbers without a point, because that is how a person writes a count
-- and a duration in seconds.  Everything else through 'show', which reads
-- back as the same double: a threshold that is a price must not be rounded,
-- and a floor of @1e-9@ that became zero would turn a division into an
-- infinity.
renderNumber :: Double -> Text
renderNumber x
  | fraction == 0 = Text.pack (show @Integer whole)
  | otherwise = Text.pack (show @Double x)
  where
    whole :: Integer
    fraction :: Double
    (whole, fraction) = properFraction x
