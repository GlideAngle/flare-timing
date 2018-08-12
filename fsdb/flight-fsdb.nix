{ mkDerivation, aeson, base, containers, detour-via-sci
, flight-comp, flight-gap, flight-latlng, flight-units, flight-zone
, hxt, hxt-xpath, megaparsec, newtype, path, scientific, smallcheck
, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, time, uom-plugin
}:
mkDerivation {
  pname = "flight-fsdb";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers detour-via-sci flight-comp flight-gap
    flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
    newtype path scientific split time uom-plugin
  ];
  testHaskellDepends = [
    aeson base containers detour-via-sci flight-comp flight-gap
    flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
    newtype path scientific smallcheck split tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck time uom-plugin
  ];
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A parser for fsdb, the database XML format of FS";
  license = stdenv.lib.licenses.mpl20;
}
