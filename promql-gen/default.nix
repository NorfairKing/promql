{ mkDerivation, base, genvalidity, genvalidity-sydtest
, genvalidity-text, lib, promql, QuickCheck, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "promql-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-text promql QuickCheck text
  ];
  testHaskellDepends = [
    base genvalidity-sydtest promql QuickCheck sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/promql#readme";
  description = "Generators and tests for promql";
  license = lib.licenses.mit;
}
