{ mkDerivation, aeson, base, containers, detour-via-sci
, detour-via-uom, flight-units, newtype, scientific, siggy-chardust
, smallcheck, statistics, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, template-haskell, uom-plugin
, vector
}:
mkDerivation {
  pname = "flight-gap";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers detour-via-sci detour-via-uom flight-units
    newtype scientific siggy-chardust statistics template-haskell
    uom-plugin vector
  ];
  testHaskellDepends = [
    aeson base containers detour-via-sci detour-via-uom flight-units
    newtype scientific siggy-chardust smallcheck statistics tasty
    tasty-hunit tasty-quickcheck tasty-smallcheck template-haskell
    uom-plugin vector
  ];
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "GAP Scoring";
  license = stdenv.lib.licenses.mpl20;
}
