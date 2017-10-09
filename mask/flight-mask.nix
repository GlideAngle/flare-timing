{ mkDerivation, base, bytestring, cmdargs, containers, directory
, fgl, filepath, flight-comp, flight-gap, flight-kml, flight-latlng
, flight-task, flight-track, flight-units, flight-zone, hlint, lens
, mtl, path, siggy-chardust, split, stdenv, time, uom-plugin, yaml
}:
mkDerivation {
  pname = "flight-mask";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cmdargs containers directory fgl filepath
    flight-comp flight-gap flight-kml flight-latlng flight-task
    flight-track flight-units flight-zone lens mtl path siggy-chardust
    split time uom-plugin yaml
  ];
  testHaskellDepends = [
    base bytestring cmdargs containers directory fgl filepath
    flight-comp flight-gap flight-kml flight-latlng flight-task
    flight-track flight-units flight-zone hlint lens mtl siggy-chardust
    split time uom-plugin yaml
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
  description = "Track logs masked by competition task zones";
  license = stdenv.lib.licenses.bsd3;
}
