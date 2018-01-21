{ mkDerivation, aeson, aeson-via-sci, base, containers, flight-comp
, flight-gap, flight-latlng, flight-units, flight-zone, hlint, hxt
, hxt-xpath, megaparsec, parsec, path, raw-strings-qq, scientific
, smallcheck, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, time, uom-plugin
}:
mkDerivation {
  pname = "flight-fsdb";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base containers flight-comp flight-gap
    flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
    path scientific split time uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base containers flight-comp flight-latlng
    flight-units flight-zone hlint hxt hxt-xpath megaparsec parsec path
    raw-strings-qq scientific smallcheck split tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck time
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-fsdb#readme";
  description = "A parser for fsdb, the database XML format of FS";
  license = stdenv.lib.licenses.bsd3;
}
