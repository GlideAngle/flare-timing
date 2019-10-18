{ mkDerivation, aeson, base, bifunctors, bytestring, cassava
, detour-via-sci, detour-via-uom, flight-units, formatting, newtype
, numbers, random, siggy-chardust, smallcheck, stdenv
, tasty-quickcheck, text, uom-plugin
}:
mkDerivation {
  pname = "flight-latlng";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors bytestring cassava detour-via-sci
    detour-via-uom flight-units formatting newtype numbers random
    siggy-chardust smallcheck tasty-quickcheck text uom-plugin
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Latitude and longitude as used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
