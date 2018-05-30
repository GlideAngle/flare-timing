{ mkDerivation, base, hlint, smallcheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "siggy-chardust";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base hlint smallcheck tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  homepage = "https://github.com/BlockScope/haskell-siggy-chardust#readme";
  description = "Rounding keeping decimal places and significant digits";
  license = stdenv.lib.licenses.mpl20;
}
