{ mkDerivation, aeson, aeson-via, aeson-via-sci, base, bytestring
, cassava, containers, directory, filemanip, filepath, flight-comp
, flight-gap, flight-latlng, flight-route, flight-zone, hlint, mtl
, path, scientific, split, stdenv, time, unordered-containers
, vector, yaml
}:
mkDerivation {
  pname = "flight-scribe";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base bytestring cassava containers directory
    filemanip filepath flight-comp flight-gap flight-latlng
    flight-route flight-zone mtl path scientific split time
    unordered-containers vector yaml
  ];
  testHaskellDepends = [
    aeson aeson-via base bytestring cassava containers directory
    filemanip filepath flight-comp flight-gap flight-latlng
    flight-route flight-zone hlint mtl path scientific split time
    unordered-containers vector yaml
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Hang gliding and paragliding competition scoring files";
  license = stdenv.lib.licenses.bsd3;
}
