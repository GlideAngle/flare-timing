{ mkDerivation, aeson-via-sci, base, bytestring, cmdargs
, containers, directory, fgl, filepath, flight-comp, flight-gap
, flight-kml, flight-latlng, flight-route, flight-scribe
, flight-span, flight-task, flight-track, flight-units, flight-zone
, hlint, lens, mtl, numbers, path, siggy-chardust, split, stdenv
, time, uom-plugin, yaml
}:
mkDerivation {
  pname = "flight-mask";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-via-sci base bytestring cmdargs containers directory fgl
    filepath flight-comp flight-gap flight-kml flight-latlng
    flight-route flight-scribe flight-span flight-task flight-track
    flight-units flight-zone lens mtl numbers path siggy-chardust split
    time uom-plugin yaml
  ];
  testHaskellDepends = [
    aeson-via-sci base bytestring cmdargs containers directory fgl
    filepath flight-comp flight-gap flight-kml flight-latlng
    flight-route flight-scribe flight-span flight-task flight-track
    flight-units flight-zone hlint lens mtl numbers siggy-chardust
    split time uom-plugin yaml
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
  description = "Track logs masked by competition task zones";
  license = stdenv.lib.licenses.bsd3;
}
