{-# LANGUAGE OverloadedStrings #-}

-- | The renderer against Prometheus's own parser.
--
-- Nothing in this repository can say whether a query is valid PromQL, or
-- whether two spellings of one mean the same thing.  Prometheus can, so these
-- ask it.
module PromQL.E2E.RenderSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import PromQL
import PromQL.E2E.Prometheus
import PromQL.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  setupAround
    ( liftIO $ do
        manager <- newManager defaultManagerSettings
        prometheus <- prometheusUrlFromEnvironment
        pure (manager, prometheus)
    )
    $ do
      it "writes a query Prometheus can parse, whatever the expression" $ \(manager, prometheus) ->
        forAllValid $ \expr -> do
          parsed <- parseQuery manager prometheus (render expr)
          case parsed of
            Parsed _ -> pure ()
            Unparseable complaint ->
              expectationFailure $
                unlines
                  [ "Prometheus would not parse what we wrote:",
                    show (render expr),
                    "",
                    show complaint
                  ]

      -- The precedence table in the renderer, checked against the one
      -- Prometheus implements.  Writing every bracket the grammar allows says
      -- exactly which tree is meant; leaving out the ones it does not need
      -- has to mean the same, and where it does not, our table is wrong.
      it "means the same with the brackets left out as with all of them in" $ \(manager, prometheus) ->
        forAllValid $ \expr -> do
          minimal <- parseQuery manager prometheus (render expr)
          everything <- parseQuery manager prometheus (renderBracketed expr)
          -- Both parsing is asserted rather than assumed: two queries that
          -- Prometheus refuses for the same reason are equal as well, and
          -- this would pass on them while saying nothing.
          minimal `shouldSatisfy` wasParsed
          minimal `shouldBe` everything

      -- Breaking a query across lines is a question of where the spaces go,
      -- and Prometheus reads nothing into those.
      it "means the same laid out across lines as on one" $ \(manager, prometheus) ->
        forAllValid $ \expr -> do
          laidOut <- parseQuery manager prometheus (render expr)
          flat <- parseQuery manager prometheus (renderFlat expr)
          laidOut `shouldSatisfy` wasParsed
          laidOut `shouldBe` flat

      -- Every property above asserts that Prometheus parsed what it was
      -- given, so none of them ever sees a refusal, and the half of the
      -- envelope that carries one would go unread.  A refusal that failed to
      -- decode now throws rather than coming back, so this is what keeps a
      -- rejected query reading as a rejected query.
      it "brings back what Prometheus said was wrong rather than throwing" $ \(manager, prometheus) -> do
        -- Well formed, and refused: log2 takes an instant vector.
        refused <- parseQuery manager prometheus (render (Apply (Log2 (Number 2))))
        refused `shouldSatisfy` (not . wasParsed)
