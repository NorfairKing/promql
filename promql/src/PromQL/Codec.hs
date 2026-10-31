{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Expressions and their parts in and out of JSON and YAML, as the text a
-- query writes them as.
--
-- Here rather than beside the types, because the codec for an expression is
-- the renderer one way and the parser the other, and both of those are built
-- on the types.
--
-- Decoding refuses what is not valid, so that a decoded value is one that
-- every other part of this library can take.
module PromQL.Codec () where

import Autodocodec
import Data.Bifunctor (first)
import Data.Validity
import PromQL.Expr
import PromQL.Parse
import PromQL.Render

-- | An expression decodes as one 'render' writes the same way, which is not
-- always the same one: every 'Regexp' reads back as a 'Pattern'.
instance HasCodec Expr where
  codec = bimapCodec (\text -> first renderParseError (parseExpr text) >>= prettyValidate) render textCodec <?> "A PromQL expression"

instance HasCodec Duration where
  codec = bimapCodec (\text -> first renderParseError (parseDuration text) >>= prettyValidate) renderDuration textCodec <?> "A PromQL duration"

instance HasCodec LabelName where
  codec = bimapCodec (prettyValidate . LabelName) unLabelName textCodec <?> "A label name"

instance HasCodec MetricName where
  codec = bimapCodec (prettyValidate . MetricName) unMetricName textCodec <?> "A metric name"
