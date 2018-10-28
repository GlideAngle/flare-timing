{ mkDerivation, base, call-stack, stdenv, tasty, tasty-hunit }:
mkDerivation {
  pname = "tasty-compare";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base call-stack tasty tasty-hunit ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Tasty HUnit extensions for comparisons";
  license = stdenv.lib.licenses.mpl20;
}
