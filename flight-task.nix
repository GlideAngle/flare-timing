{ mkDerivation, base, fgl, hlint, numbers, siggy-chardust
, smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck
}:
mkDerivation {
  pname = "flight-task";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base fgl numbers ];
  testHaskellDepends = [
    base fgl hlint numbers siggy-chardust smallcheck tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Tasks to fly";
  license = stdenv.lib.licenses.bsd3;
}
