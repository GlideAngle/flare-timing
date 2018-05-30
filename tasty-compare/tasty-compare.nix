{ mkDerivation, base, call-stack, hlint, stdenv, tasty, tasty-hunit
}:
mkDerivation {
  pname = "tasty-compare";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base call-stack tasty tasty-hunit ];
  testHaskellDepends = [ base call-stack hlint tasty tasty-hunit ];
  homepage = "https://github.com/BlockScope/tasty-compare#readme";
  description = "Tasty HUnit extensions for comparisons";
  license = stdenv.lib.licenses.mpl20;
}
