{ mkDerivation, base, data-default, hcoord, hlint, hpack, HUnit
, ieee754, mtl, stdenv
}:
mkDerivation {
  pname = "hcoord-utm";
  version = "2.0.0";
  src = ../.stack2cabal/hcoord;
  postUnpack = "sourceRoot+=/hcoord-utm; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base hcoord mtl ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base data-default hcoord hlint HUnit ieee754 mtl
  ];
  doHaddock = false;
  doCheck = false;
  preConfigure = "hpack";
  homepage = "https://github.com/danfran/hcoord#readme";
  description = "Short synopsis";
  license = stdenv.lib.licenses.bsd3;
}
