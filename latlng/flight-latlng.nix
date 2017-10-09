{ mkDerivation, aeson, base, flight-units, hlint, numbers
, scientific, siggy-chardust, stdenv, uom-plugin
}:
mkDerivation {
  pname = "flight-latlng";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base flight-units numbers scientific siggy-chardust
    uom-plugin
  ];
  testHaskellDepends = [
    aeson base flight-units hlint numbers scientific siggy-chardust
    uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Latitude and longitude as used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.bsd3;
}
