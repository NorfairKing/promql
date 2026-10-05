-- | PromQL as a type rather than as text.
--
-- Build an expression with "PromQL.Expr" and write it out with
-- "PromQL.Render".  Nothing else in this library writes any PromQL, so
-- 'PromQL.Render.render' is the only place that has to be right about the
-- grammar.
module PromQL
  ( module PromQL.Expr,
    module PromQL.Render,
  )
where

import PromQL.Expr
import PromQL.Render
