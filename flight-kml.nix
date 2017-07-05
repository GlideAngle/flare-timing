{ mkDerivation, base, hlint, hxt, hxt-xpath, parsec, path
, raw-strings-qq, smallcheck, split, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "flight-kml";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base hxt hxt-xpath parsec path split ];
  testHaskellDepends = [
    base hlint hxt hxt-xpath parsec path raw-strings-qq smallcheck
    split tasty tasty-hunit tasty-quickcheck tasty-smallcheck
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-kml#readme";
  description = "A parser for KML files";
  license = stdenv.lib.licenses.bsd3;
}
