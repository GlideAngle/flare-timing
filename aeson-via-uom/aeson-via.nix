{ mkDerivation, aeson, base, cassava, hlint, newtype, scientific
, stdenv
}:
mkDerivation {
  pname = "aeson-via";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base cassava newtype scientific ];
  testHaskellDepends = [
    aeson base cassava hlint newtype scientific
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "JSON encoding and decoding for rationals via scientific";
  license = stdenv.lib.licenses.mpl20;
}
