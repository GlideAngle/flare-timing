{ mkDerivation, aeson, base, bifunctors, detour-via-sci
, flight-earth, flight-latlng, flight-task, flight-units
, flight-zone, hcoord-utm, numbers, scientific, siggy-chardust
, stdenv, uom-plugin
}:
mkDerivation {
  pname = "flight-route";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors detour-via-sci flight-earth flight-latlng
    flight-task flight-units flight-zone hcoord-utm numbers scientific
    siggy-chardust uom-plugin
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Control zones to fly";
  license = stdenv.lib.licenses.mpl20;
}
