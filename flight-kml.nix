{ mkDerivation, aeson, base, hlint, hxt, hxt-xpath, parsec, path
, raw-strings-qq, smallcheck, split, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, time
}:
mkDerivation {
  pname = "flight-kml";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base hxt hxt-xpath parsec path split time
  ];
  testHaskellDepends = [
    aeson base hlint hxt hxt-xpath parsec path raw-strings-qq
    smallcheck split tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck time
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-kml#readme";
  description = "A parser for KML files";
  license = stdenv.lib.licenses.bsd3;
}
