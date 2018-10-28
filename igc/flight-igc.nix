{ mkDerivation, base, bytestring, parsec, stdenv, utf8-string }:
mkDerivation {
  pname = "flight-igc";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring parsec utf8-string ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A parser for IGC files";
  license = stdenv.lib.licenses.mpl20;
}
