{ mkDerivation, aeson, aeson-via-sci, base, bifunctors, fgl
, flight-latlng, flight-units, flight-zone, hcoord, hlint, mtl
, numbers, scientific, siggy-chardust, smallcheck, stdenv, tasty
, tasty-compare, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, uom-plugin
}:
mkDerivation {
  pname = "flight-sphere";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
    flight-zone hcoord mtl numbers scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
    flight-zone hcoord hlint mtl numbers scientific siggy-chardust
    smallcheck tasty tasty-compare tasty-hunit tasty-quickcheck
    tasty-smallcheck uom-plugin
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Distances on the FAI sphere";
  license = stdenv.lib.licenses.bsd3;
}
