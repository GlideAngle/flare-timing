{ mkDerivation, base, doctest, split, stdenv, time }:
mkDerivation {
  pname = "flight-clip";
  version = "1.1.0";
  src = ./.;
  libraryHaskellDepends = [ base split time ];
  testHaskellDepends = [ base doctest split time ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing/tree/master/clip#readme";
  description = "Clipping a pilot's tracklogs";
  license = stdenv.lib.licenses.mpl20;
}
