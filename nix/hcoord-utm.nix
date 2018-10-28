{ mkDerivation, base, data-default, fetchgit, hcoord, hlint, HUnit
, ieee754, mtl, stdenv
}:
mkDerivation {
  pname = "hcoord-utm";
  version = "2.1.0";
  src = fetchgit {
    url = "http://github.com/BlockScope/hcoord.git";
    sha256 = "0267n694m08bv73ld7f5flb66h3dxc7xgrbmkr757q0g87l8ndzq";
    rev = "3c3859dac5da111e57a6de09764ffdb127197c4a";
  };
  postUnpack = "sourceRoot+=/hcoord-utm; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base hcoord mtl ];
  testHaskellDepends = [
    base data-default hcoord hlint HUnit ieee754 mtl
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/danfran/hcoord#readme";
  description = "Short synopsis";
  license = stdenv.lib.licenses.bsd3;
}
