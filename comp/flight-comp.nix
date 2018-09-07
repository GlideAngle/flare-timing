{ mkDerivation, aeson, base, bytestring, cassava, containers
, detour-via-sci, directory, filemanip, filepath, flight-gap
, flight-latlng, flight-route, flight-units, flight-zone, lens, mtl
, path, scientific, smallcheck, split, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, time, unordered-containers
, uom-plugin, vector
}:
mkDerivation {
  pname = "flight-comp";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cassava containers detour-via-sci directory
    filemanip filepath flight-gap flight-latlng flight-route
    flight-units flight-zone lens mtl path scientific split time
    unordered-containers uom-plugin vector
  ];
  testHaskellDepends = [
    aeson base bytestring cassava containers detour-via-sci directory
    filemanip filepath flight-gap flight-latlng flight-route
    flight-units flight-zone lens mtl path scientific smallcheck split
    tasty tasty-hunit tasty-quickcheck tasty-smallcheck time
    unordered-containers uom-plugin vector
  ];
  doCheck = false;
  doHaddock = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Hang gliding and paragliding competition scoring inputs";
  license = stdenv.lib.licenses.mpl20;
}
