{ mkDerivation, base, bytestring, doctest, flight-clip, megaparsec
, stdenv, tasty-quickcheck, time, utf8-string
}:
mkDerivation {
  pname = "flight-igc";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring flight-clip megaparsec tasty-quickcheck time
    utf8-string
  ];
  testHaskellDepends = [
    base bytestring doctest flight-clip megaparsec tasty-quickcheck
    time utf8-string
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A parser for IGC files";
  license = stdenv.lib.licenses.mpl20;
}
