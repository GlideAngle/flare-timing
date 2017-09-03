{ mkDerivation, base, bytestring, containers, directory, fgl
, filepath, flight-comp, flight-gap, flight-kml, flight-task
, flight-track, flight-units, hlint, lens, mtl, path
, siggy-chardust, split, stdenv, time, uom-plugin, yaml
}:
mkDerivation {
  pname = "flight-mask";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory fgl filepath flight-comp
    flight-gap flight-kml flight-task flight-track flight-units lens
    mtl path siggy-chardust split time uom-plugin yaml
  ];
  testHaskellDepends = [
    base bytestring containers directory fgl filepath flight-comp
    flight-gap flight-kml flight-task flight-track flight-units hlint
    lens mtl siggy-chardust split time uom-plugin yaml
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
  description = "Track logs masked by competition task zones";
  license = stdenv.lib.licenses.bsd3;
}
