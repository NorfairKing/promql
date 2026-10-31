{ mkDerivation, autodocodec, base, bytestring, lib, megaparsec
, scientific, text, validity, validity-text
}:
mkDerivation {
  pname = "promql";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base bytestring megaparsec scientific text validity
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/promql#readme";
  description = "PromQL as a type rather than as text";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
