{ mkDerivation, base, hlint, parsec, stdenv }:
mkDerivation {
  pname = "flight-igc";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base parsec ];
  testHaskellDepends = [ base hlint ];
  homepage = "https://github.com/BlockScope/haskell-flight-igc#readme";
  description = "A parser for IGC files";
  license = stdenv.lib.licenses.mpl20;
}
