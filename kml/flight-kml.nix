{ mkDerivation, aeson, base, detour-via-sci, doctest, hxt
, hxt-xpath, megaparsec, path, raw-strings-qq, siggy-chardust
, smallcheck, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, template-haskell, time
}:
mkDerivation {
  pname = "flight-kml";
  version = "1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base detour-via-sci hxt hxt-xpath megaparsec path
    siggy-chardust split time
  ];
  testHaskellDepends = [
    aeson base detour-via-sci doctest hxt hxt-xpath megaparsec path
    raw-strings-qq siggy-chardust smallcheck split tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck template-haskell time
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing/tree/master/kml#readme";
  description = "Parsing of pilot tracklogs dumped as KML";
  license = stdenv.lib.licenses.mpl20;
}
