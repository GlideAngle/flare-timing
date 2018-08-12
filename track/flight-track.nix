{ mkDerivation, base, bytestring, containers, directory, filepath
, flight-comp, flight-igc, flight-kml, mtl, path, split, stdenv
, time, utf8-string
}:
mkDerivation {
  pname = "flight-track";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath flight-comp
    flight-igc flight-kml mtl path split time utf8-string
  ];
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Hang gliding and paragliding competition track logs";
  license = stdenv.lib.licenses.mpl20;
}
