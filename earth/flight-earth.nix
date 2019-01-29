{ mkDerivation, aeson, base, bifunctors, detour-via-sci
, detour-via-uom, fgl, flight-latlng, flight-units, flight-zone
, hcoord, hcoord-utm, mtl, numbers, scientific, siggy-chardust
, smallcheck, stdenv, tasty, tasty-compare, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, uom-plugin
}:
mkDerivation {
  pname = "flight-earth";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors detour-via-sci detour-via-uom fgl
    flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
    numbers scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson base bifunctors detour-via-sci detour-via-uom fgl
    flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
    numbers scientific siggy-chardust smallcheck tasty tasty-compare
    tasty-hunit tasty-quickcheck tasty-smallcheck uom-plugin
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Distances on the WGS84 ellipsoid, the FAI sphere and the UTM projection";
  license = stdenv.lib.licenses.mpl20;
}
