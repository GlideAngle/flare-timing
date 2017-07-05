{ mkDerivation, aeson, ansi-terminal, base, cmdargs, directory
, filemanip, filepath, flight-fsdb, flight-igc, flight-kml, hlint
, mtl, raw-strings-qq, servant, servant-server, shake, stdenv
, system-filepath, time, transformers, wai, wai-cors, warp
}:
mkDerivation {
  pname = "flare-timing";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base cmdargs directory filemanip filepath
    flight-fsdb flight-igc flight-kml mtl raw-strings-qq servant
    servant-server shake system-filepath time transformers wai wai-cors
    warp
  ];
  testHaskellDepends = [ base hlint ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.bsd3;
}
