{ mkDerivation, aeson, aeson-via, base, bifunctors, flight-latlng
, flight-units, hlint, scientific, siggy-chardust, stdenv
, uom-plugin
}:
mkDerivation {
  pname = "flight-zone";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via base bifunctors flight-latlng flight-units
    scientific siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via base bifunctors flight-latlng flight-units hlint
    scientific siggy-chardust uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Control zones to fly";
  license = stdenv.lib.licenses.bsd3;
}
