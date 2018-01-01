{ mkDerivation, aeson, aeson-via, base, bytestring, cassava
, containers, directory, filemanip, filepath, flight-gap
, flight-latlng, flight-route, flight-units, flight-zone, hlint
, lens, mtl, path, scientific, split, stdenv, time
, unordered-containers, uom-plugin, vector
}:
mkDerivation {
  pname = "flight-comp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via base bytestring cassava containers directory
    filemanip filepath flight-gap flight-latlng flight-route
    flight-units flight-zone lens mtl path scientific split time
    unordered-containers uom-plugin vector
  ];
  testHaskellDepends = [
    aeson aeson-via base bytestring cassava containers directory
    filemanip filepath flight-gap flight-latlng flight-route
    flight-zone hlint lens mtl path scientific split time
    unordered-containers uom-plugin vector
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Hang gliding and paragliding competition scoring inputs";
  license = stdenv.lib.licenses.bsd3;
}
