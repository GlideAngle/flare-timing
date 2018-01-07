{ mkDerivation, aeson, aeson-via-sci, base, flight-units, hlint
, newtype, scientific, stdenv, uom-plugin
}:
mkDerivation {
  pname = "aeson-via-uom";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base flight-units newtype scientific uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base flight-units hlint newtype scientific
    uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "JSON encoding and decoding for rationals via scientific";
  license = stdenv.lib.licenses.bsd3;
}
