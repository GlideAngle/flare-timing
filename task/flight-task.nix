{ mkDerivation, base, bifunctors, fgl, flight-units, hlint, numbers
, siggy-chardust, smallcheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, uom-plugin
}:
mkDerivation {
  pname = "flight-task";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors fgl flight-units numbers siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    base bifunctors fgl flight-units hlint numbers siggy-chardust
    smallcheck tasty tasty-hunit tasty-quickcheck tasty-smallcheck
    uom-plugin
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Tasks to fly";
  license = stdenv.lib.licenses.bsd3;
}
