{ mkDerivation, base, cmdargs, hlint, stdenv }:
mkDerivation {
  pname = "flight-span";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base cmdargs ];
  testHaskellDepends = [ base cmdargs hlint ];
  homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
  description = "How to measure a distance that spans two points";
  license = stdenv.lib.licenses.bsd3;
}
