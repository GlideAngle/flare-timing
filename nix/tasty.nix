{ mkDerivation, ansi-terminal, async, base, clock, containers, mtl
, optparse-applicative, stdenv, stm, tagged, unbounded-delays, unix
}:
mkDerivation {
  pname = "tasty";
  version = "1.0.1.1";
  sha256 = "1a920d41a58402ee93a4b060c3f2792a9b9a444a049ecc58a72d2d3eeadbeeb3";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers mtl optparse-applicative
    stm tagged unbounded-delays unix
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
