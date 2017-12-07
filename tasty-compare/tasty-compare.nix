{ mkDerivation, base, hlint, smallcheck, stdenv, tasty, tasty-hunit
}:
mkDerivation {
  pname = "tasty-compare";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base hlint smallcheck tasty tasty-hunit
  ];
  homepage = "https://github.com/BlockScope/haskell-tasty-compare#readme";
  description = "Adds assertCompare and related operators";
  license = stdenv.lib.licenses.bsd3;
}
