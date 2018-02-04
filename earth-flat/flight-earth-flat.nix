{ mkDerivation, aeson, aeson-via-sci, base, bifunctors, fgl
, flight-latlng, flight-units, flight-zone, hcoord, hlint, mtl
, numbers, scientific, siggy-chardust, stdenv, tasty-compare
, uom-plugin
}:
mkDerivation {
  pname = "flight-earth-flat";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
    flight-zone hcoord mtl numbers scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
    flight-zone hcoord hlint mtl numbers scientific siggy-chardust
    tasty-compare uom-plugin
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Distances on a UTM projection";
  license = stdenv.lib.licenses.bsd3;
}
