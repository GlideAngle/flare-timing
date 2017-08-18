{ mkDerivation, base, containers, directory, filepath, flight-comp
, flight-kml, hlint, mtl, path, split, stdenv
}:
mkDerivation {
  pname = "flight-mask";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers directory filepath flight-comp flight-kml mtl path
    split
  ];
  testHaskellDepends = [
    base containers directory filepath flight-comp flight-kml hlint mtl
    split
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
  description = "Track logs masked by competition task zones";
  license = stdenv.lib.licenses.bsd3;
}
