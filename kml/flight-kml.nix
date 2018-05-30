{ mkDerivation, aeson, aeson-via-sci, base, hlint, hxt, hxt-xpath
, parsec, path, raw-strings-qq, siggy-chardust, smallcheck, split
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, time
}:
mkDerivation {
  pname = "flight-kml";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base hxt hxt-xpath parsec path siggy-chardust
    split time
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base hlint hxt hxt-xpath parsec path
    raw-strings-qq siggy-chardust smallcheck split tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck time
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-kml#readme";
  description = "A parser for KML files";
  license = stdenv.lib.licenses.mpl20;
}
