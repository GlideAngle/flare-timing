{ mkDerivation, aeson, base, bytestring, cmdargs, directory
, filemanip, filepath, flight-comp, flight-fsdb, flight-gap
, flight-igc, flight-kml, flight-mask, flight-task, flight-track
, flight-units, hlint, mtl, raw-strings-qq, servant, servant-server
, siggy-chardust, stdenv, system-filepath, transformers, uom-plugin
, wai, wai-cors, warp, yaml
}:
mkDerivation {
  pname = "flare-timing";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cmdargs directory filemanip filepath
    flight-comp flight-fsdb flight-gap flight-igc flight-kml
    flight-mask flight-task flight-track flight-units mtl
    raw-strings-qq servant servant-server siggy-chardust
    system-filepath transformers uom-plugin wai wai-cors warp yaml
  ];
  testHaskellDepends = [ base hlint ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
