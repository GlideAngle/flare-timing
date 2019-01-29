{ mkDerivation, aeson, base, cassava, doctest, newtype, scientific
, siggy-chardust, stdenv, template-haskell
}:
mkDerivation {
  pname = "detour-via-sci";
  version = "1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base cassava newtype scientific siggy-chardust
    template-haskell
  ];
  testHaskellDepends = [
    aeson base cassava doctest newtype scientific siggy-chardust
    template-haskell
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-sci#readme";
  description = "JSON and CSV encoding for rationals as decimal point numbers";
  license = stdenv.lib.licenses.mpl20;
}
