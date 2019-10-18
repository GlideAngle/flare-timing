{ mkDerivation, aeson, base, cassava, detour-via-sci, doctest
, newtype, scientific, stdenv, uom-plugin
}:
mkDerivation {
  pname = "detour-via-uom";
  version = "1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base cassava detour-via-sci newtype scientific uom-plugin
  ];
  testHaskellDepends = [
    aeson base cassava detour-via-sci doctest newtype scientific
    uom-plugin
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-uom#readme";
  description = "JSON and CSV encoding for quantities";
  license = stdenv.lib.licenses.mpl20;
}
