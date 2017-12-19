{ mkDerivation, aeson, aeson-via, base, bifunctors, bytestring
, cassava, flight-units, formatting, hlint, newtype, numbers
, siggy-chardust, stdenv, text, uom-plugin
}:
mkDerivation {
  pname = "flight-latlng";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via base bifunctors bytestring cassava flight-units
    formatting newtype numbers siggy-chardust text uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via base bifunctors bytestring cassava flight-units
    formatting hlint newtype numbers siggy-chardust text uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Latitude and longitude as used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.bsd3;
}
