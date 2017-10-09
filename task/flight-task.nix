{ mkDerivation, aeson, base, bifunctors, fgl, flight-latlng
, flight-units, hlint, numbers, scientific, siggy-chardust
, smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, uom-plugin
}:
mkDerivation {
  pname = "flight-task";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors fgl flight-latlng flight-units numbers
    scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson base bifunctors fgl flight-latlng flight-units hlint numbers
    scientific siggy-chardust smallcheck tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck uom-plugin
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Tasks to fly";
  license = stdenv.lib.licenses.bsd3;
}
