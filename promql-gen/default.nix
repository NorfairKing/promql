{ mkDerivation, aeson, autodocodec, base, containers, genvalidity
, genvalidity-sydtest, genvalidity-text, lib, megaparsec, promql
, QuickCheck, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "promql-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-text promql QuickCheck text
  ];
  testHaskellDepends = [
    aeson autodocodec base containers genvalidity-sydtest megaparsec
    promql QuickCheck sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/promql#readme";
  description = "Generators and tests for promql";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
