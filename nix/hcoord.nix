{ mkDerivation, base, data-default, hlint, HUnit, ieee754, mtl
, stdenv
}:
mkDerivation {
  pname = "hcoord";
  version = "2.1.0";
  src = ../.stack2cabal/hcoord;
  postUnpack = "sourceRoot+=/hcoord; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base data-default hlint HUnit ieee754 mtl ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/danfran/hcoord#readme";
  description = "Short synopsis";
  license = stdenv.lib.licenses.bsd3;
}
