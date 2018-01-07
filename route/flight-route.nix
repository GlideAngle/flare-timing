{ mkDerivation, aeson, aeson-via-sci, base, bifunctors
, flight-latlng, flight-task, flight-units, flight-zone, hcoord
, hlint, numbers, scientific, siggy-chardust, stdenv, uom-plugin
}:
mkDerivation {
  pname = "flight-route";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-via-sci base bifunctors flight-latlng flight-task
    flight-units flight-zone hcoord numbers scientific siggy-chardust
    uom-plugin
  ];
  testHaskellDepends = [
    aeson aeson-via-sci base bifunctors flight-latlng flight-task
    flight-units flight-zone hcoord hlint numbers scientific
    siggy-chardust uom-plugin
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Control zones to fly";
  license = stdenv.lib.licenses.bsd3;
}
