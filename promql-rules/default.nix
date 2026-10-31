{ mkDerivation, aeson-pretty, autodocodec, base, bytestring
, containers, lib, megaparsec, path, path-io, promql, scientific
, text, typed-process, validity, validity-containers, validity-text
}:
mkDerivation {
  pname = "promql-rules";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-pretty autodocodec base bytestring containers megaparsec path
    path-io promql scientific text typed-process validity
    validity-containers validity-text
  ];
  homepage = "https://github.com/NorfairKing/promql#readme";
  description = "Prometheus rules, and unit tests for them, as types rather than as text";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
