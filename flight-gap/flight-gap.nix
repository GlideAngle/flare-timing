{ mkDerivation, base, containers, hlint, smallcheck, statistics
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, vector
}:
mkDerivation {
  pname = "flight-gap";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers statistics vector ];
  testHaskellDepends = [
    base containers hlint smallcheck statistics tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck vector
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-gap#readme";
  description = "GAP Scoring";
  license = stdenv.lib.licenses.bsd3;
}
