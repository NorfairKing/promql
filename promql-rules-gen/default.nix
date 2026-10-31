{ mkDerivation, aeson, autodocodec, base, containers, genvalidity
, genvalidity-containers, genvalidity-sydtest, genvalidity-text
, lib, path, promql, promql-gen, promql-rules, QuickCheck, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "promql-rules-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers genvalidity genvalidity-containers genvalidity-text
    promql promql-gen promql-rules QuickCheck sydtest text
  ];
  testHaskellDepends = [
    aeson autodocodec base containers genvalidity-sydtest path promql
    promql-rules sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/promql#readme";
  description = "Generators and tests for promql-rules";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
