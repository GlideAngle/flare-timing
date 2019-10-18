{ mkDerivation, base, doctest, smallcheck, stdenv, tasty
, tasty-hunit, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "siggy-chardust";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing/tree/master/siggy-chardust#readme";
  description = "Rounding rationals to significant digits and decimal places";
  license = stdenv.lib.licenses.mpl20;
}
