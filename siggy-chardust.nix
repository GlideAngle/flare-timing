{ mkDerivation, base, hlint, stdenv, tasty, tasty-hunit }:
mkDerivation {
  pname = "siggy-chardust";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hlint tasty tasty-hunit ];
  homepage = "https://github.com/BlockScope/haskell-siggy-chardust#readme";
  description = "Rounding keeping decimal places and significant digits";
  license = stdenv.lib.licenses.bsd3;
}
