{ mkDerivation, aeson, aeson-via, base, bytestring, cassava
, containers, directory, filemanip, filepath, flight-comp
, flight-gap, flight-kml, flight-latlng, flight-mask, flight-zone
, hcoord, hlint, lens, mtl, path, scientific, split, stdenv, time
, unordered-containers, uom-plugin
}:
mkDerivation {
  pname = "flight-lookup";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via base bytestring cassava containers directory
    filemanip filepath flight-comp flight-gap flight-kml flight-latlng
    flight-mask flight-zone hcoord lens mtl path scientific split time
    unordered-containers uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via base bytestring cassava containers directory
    filemanip filepath flight-comp flight-gap flight-kml flight-latlng
    flight-mask flight-zone hcoord hlint lens mtl path scientific split
    time unordered-containers uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Hang gliding and paragliding competition data access";
  license = stdenv.lib.licenses.bsd3;
}
