{ mkDerivation, aeson, base, bytestring, clock, cmdargs, containers
, directory, filemanip, filepath, flight-cmd, flight-comp
, flight-earth, flight-fsdb, flight-gap, flight-igc, flight-kml
, flight-latlng, flight-lookup, flight-mask, flight-route
, flight-scribe, flight-span, flight-time, flight-units
, flight-zone, formatting, lens, mtl, raw-strings-qq
, safe-exceptions, siggy-chardust, stdenv, time, transformers
, uom-plugin, yaml
}:
mkDerivation {
  pname = "flare-timing";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring clock cmdargs containers directory filemanip
    filepath flight-cmd flight-comp flight-earth flight-fsdb flight-gap
    flight-igc flight-kml flight-latlng flight-lookup flight-mask
    flight-route flight-scribe flight-span flight-time flight-units
    flight-zone formatting lens mtl raw-strings-qq safe-exceptions
    siggy-chardust time transformers uom-plugin yaml
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
