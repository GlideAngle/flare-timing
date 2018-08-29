{ mkDerivation, aeson, base, bifunctors, detour-via-sci, fgl
, flight-earth, flight-latlng, flight-units, flight-zone, mtl
, numbers, scientific, siggy-chardust, smallcheck, stdenv, tasty
, tasty-compare, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, uom-plugin
}:
mkDerivation {
  pname = "flight-task";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors detour-via-sci fgl flight-earth flight-latlng
    flight-units flight-zone mtl numbers scientific siggy-chardust
    uom-plugin
  ];
  testHaskellDepends = [
    aeson base bifunctors detour-via-sci fgl flight-earth flight-latlng
    flight-units flight-zone mtl numbers scientific siggy-chardust
    smallcheck tasty tasty-compare tasty-hunit tasty-quickcheck
    tasty-smallcheck uom-plugin
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Tasks to fly";
  license = stdenv.lib.licenses.mpl20;
}
