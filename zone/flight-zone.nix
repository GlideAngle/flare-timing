{ mkDerivation, aeson, aeson-pretty, base, bytestring
, detour-via-sci, detour-via-uom, doctest, flight-latlng
, flight-units, here, newtype, scientific, siggy-chardust, stdenv
, tasty, tasty-discover, tasty-golden, tasty-hspec, text
, uom-plugin, yaml
}:
mkDerivation {
  pname = "flight-zone";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base detour-via-sci detour-via-uom flight-latlng flight-units
    newtype scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring detour-via-sci detour-via-uom
    doctest flight-latlng flight-units here newtype scientific
    siggy-chardust tasty tasty-discover tasty-golden tasty-hspec text
    uom-plugin yaml
  ];
  testToolDepends = [ tasty-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Control zones to fly";
  license = stdenv.lib.licenses.mpl20;
}
