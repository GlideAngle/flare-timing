{ mkDerivation, aeson, base, hlint, newtype, scientific, stdenv }:
mkDerivation {
  pname = "aeson-via";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base newtype scientific ];
  testHaskellDepends = [ aeson base hlint newtype scientific ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "JSON encoding and decoding for rationals via scientific";
  license = stdenv.lib.licenses.bsd3;
}
