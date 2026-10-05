{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Asking a real Prometheus what a query means.
--
-- Prometheus's own parser is the only thing that can answer that, so these
-- tests talk to one rather than to a transcription of its grammar.  The
-- @parse_query@ endpoint hands back the tree it parsed, which is what makes
-- it possible to ask whether two spellings mean the same thing; @promtool
-- promql format@ cannot, because it keeps whatever brackets it was given.
module PromQL.E2E.Prometheus
  ( PrometheusUrl (..),
    prometheusUrlFromEnvironment,
    ParsedQuery (..),
    wasParsed,
    parseQuery,
  )
where

import Autodocodec
import Data.Aeson (Value (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Network.HTTP.Client
import System.Environment (lookupEnv)

-- | Where the Prometheus these tests ask is.
newtype PrometheusUrl = PrometheusUrl {unPrometheusUrl :: String}
  deriving (Show, Eq)

-- | The Prometheus the check was started against.
--
-- These tests are only meaningful against a real server, so there is no
-- default to fall back to: without one they say so rather than passing
-- vacuously.
prometheusUrlFromEnvironment :: IO PrometheusUrl
prometheusUrlFromEnvironment =
  lookupEnv "PROMETHEUS_URL" >>= \case
    Nothing -> fail "PROMETHEUS_URL is unset, so there is no Prometheus to ask."
    Just url -> pure (PrometheusUrl url)

-- | What Prometheus made of a query.
--
-- Both of these are answers: a refusal is Prometheus saying what is wrong
-- with the query, which is as much a result as a tree.  A response that is
-- neither is not one of these, because that is a server that is not working
-- rather than a query that is not valid.
data ParsedQuery
  = -- | The tree it parsed.
    Parsed !Value
  | -- | What it said was wrong with the query.
    Unparseable !Text
  deriving (Show, Eq)

-- | The envelope the two answers arrive in, which @status@ tells apart.
--
-- No 'ToJSON' or 'FromJSON' beside this: nothing writes one of these, so an
-- encoder would be one nothing uses, and the codec is all the decoding needs.
instance HasCodec ParsedQuery where
  codec =
    object "ParsedQuery" $
      discriminatedUnionCodec
        "status"
        ( \case
            Parsed tree -> ("success", mapToEncoder tree (requiredField' "data"))
            Unparseable complaint -> ("error", mapToEncoder complaint (requiredField' "error"))
        )
        ( HashMap.fromList
            [ ("success", ("Parsed", mapToDecoder Parsed (requiredField' "data"))),
              ("error", ("Unparseable", mapToDecoder Unparseable (requiredField' "error")))
            ]
        )

-- | Whether Prometheus made anything of it at all.
--
-- A test that compares two 'ParsedQuery' values is answered by two refusals
-- as readily as by two trees, so one that means to compare trees has to say
-- that it got any.
wasParsed :: ParsedQuery -> Bool
wasParsed = \case
  Parsed _ -> True
  Unparseable _ -> False

-- | Hand a query to Prometheus and keep the tree, or the complaint.
--
-- The tree is normalised first: brackets the grammar makes redundant are
-- dropped and a selector's matchers are sorted, because a query means
-- nothing by either, and comparing two spellings is the whole point.
--
-- A response that is not one of the two answers throws rather than coming
-- back as a refusal.  A test told that Prometheus refused the query would go
-- looking at the query, and the thing to look at would be the server.
parseQuery :: Manager -> PrometheusUrl -> Text -> IO ParsedQuery
parseQuery manager prometheus query = do
  initial <- parseUrlThrow (concat [unPrometheusUrl prometheus, "/api/v1/parse_query"])
  let request =
        setQueryString
          [("query", Just (Text.encodeUtf8 query))]
          initial {checkResponse = \_ _ -> pure ()}
  response <- httpLbs request manager
  case eitherDecodeJSONViaCodec (responseBody response) of
    Left complaint ->
      fail $
        unlines
          [ "Prometheus answered neither a tree nor a complaint about the query,",
            "which is a server that is not working rather than a query that is not valid:",
            complaint,
            show (responseBody response)
          ]
    Right (Parsed tree) -> pure (Parsed (normalise tree))
    Right unparseable -> pure unparseable

-- | A parse tree with everything in it that a query does not mean.
normalise :: Value -> Value
normalise = \case
  Object fields
    | Just "parenExpr" <- KeyMap.lookup "type" fields,
      Just inner <- KeyMap.lookup "expr" fields ->
        normalise inner
    | otherwise -> Object (KeyMap.map normalise (sortMatchers fields))
  Array values -> Array (Vector.map normalise values)
  other -> other
  where
    -- A selector says the same thing whatever order its matchers are written
    -- in, and Prometheus keeps the order it was given.
    sortMatchers fields = case KeyMap.lookup "matchers" fields of
      Just (Array matchers) ->
        KeyMap.insert
          "matchers"
          (Array (Vector.fromList (sortOn JSON.encode (Vector.toList matchers))))
          fields
      _ -> fields
