{ mkDerivation, aeson, autodocodec, base, containers
, genvalidity-sydtest, http-client, lib, promql, promql-gen
, promql-rules, promql-rules-gen, QuickCheck, sydtest
, sydtest-discover, text, unordered-containers, vector
}:
mkDerivation {
  pname = "promql-e2e";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base http-client text unordered-containers vector
  ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client promql promql-gen
    promql-rules promql-rules-gen QuickCheck sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  description = "End-to-end tests for promql against a real Prometheus";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
