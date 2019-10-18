{ mkDerivation, aeson, base, bytestring, cassava, containers
, detour-via-sci, directory, filemanip, filepath, flight-clip
, flight-comp, flight-gap, flight-latlng, flight-route, flight-zone
, mtl, path, safe-exceptions, scientific, split, stdenv, time
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "flight-scribe";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cassava containers detour-via-sci directory
    filemanip filepath flight-clip flight-comp flight-gap flight-latlng
    flight-route flight-zone mtl path safe-exceptions scientific split
    time unordered-containers vector yaml
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Hang gliding and paragliding competition scoring files";
  license = stdenv.lib.licenses.mpl20;
}
