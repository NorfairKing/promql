{ mkDerivation, aeson, base, genvalidity-sydtest, http-client, lib
, promql, promql-gen, sydtest, sydtest-discover, text, vector
}:
mkDerivation {
  pname = "promql-e2e";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base http-client text vector ];
  testHaskellDepends = [
    base genvalidity-sydtest http-client promql promql-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  description = "End-to-end tests for promql against a real Prometheus";
  license = lib.licenses.mit;
}
