{ mkDerivation, base, bytestring, cmdargs, containers
, detour-via-sci, directory, fgl, filepath, flight-comp
, flight-earth, flight-gap, flight-kml, flight-latlng, flight-route
, flight-scribe, flight-span, flight-task, flight-track
, flight-units, flight-zone, lens, mtl, numbers, path
, siggy-chardust, split, stdenv, time, uom-plugin, yaml
}:
mkDerivation {
  pname = "flight-mask";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cmdargs containers detour-via-sci directory fgl
    filepath flight-comp flight-earth flight-gap flight-kml
    flight-latlng flight-route flight-scribe flight-span flight-task
    flight-track flight-units flight-zone lens mtl numbers path
    siggy-chardust split time uom-plugin yaml
  ];
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Track logs masked by competition task zones";
  license = stdenv.lib.licenses.mpl20;
}
