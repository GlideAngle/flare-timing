{ mkDerivation, aeson, base, containers, flight-comp, hlint, hxt
, hxt-xpath, parsec, path, raw-strings-qq, scientific, smallcheck
, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, time
}:
mkDerivation {
  pname = "flight-fsdb";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers flight-comp hxt hxt-xpath parsec path
    scientific split time
  ];
  testHaskellDepends = [
    aeson base containers flight-comp hlint hxt hxt-xpath parsec path
    raw-strings-qq scientific smallcheck split tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck time
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-fsdb#readme";
  description = "A parser for fsdb, the database XML format of FS";
  license = stdenv.lib.licenses.bsd3;
}
