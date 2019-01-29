{ mkDerivation, base, bytestring, doctest, megaparsec, stdenv
, utf8-string
}:
mkDerivation {
  pname = "flight-igc";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring megaparsec utf8-string ];
  testHaskellDepends = [
    base bytestring doctest megaparsec utf8-string
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A parser for IGC files";
  license = stdenv.lib.licenses.mpl20;
}
