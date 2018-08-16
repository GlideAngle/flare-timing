{ mkDerivation, base, data-default, hlint, hpack, HUnit, ieee754
, mtl, stdenv
}:
mkDerivation {
  pname = "hcoord";
  version = "2.0.0";
  src = ../.stack2cabal/hcoord/hcoord;
  libraryHaskellDepends = [ base mtl ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base data-default hlint HUnit ieee754 mtl ];
  doHaddock = false;
  doCheck = false;
  preConfigure = "hpack";
  homepage = "https://github.com/danfran/hcoord#readme";
  description = "Short synopsis";
  license = stdenv.lib.licenses.bsd3;
}
