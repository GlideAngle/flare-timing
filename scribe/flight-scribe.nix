{ mkDerivation, aeson, base, containers, flight-zone, hlint, path
, scientific, split, stdenv, time
}:
mkDerivation {
  pname = "flight-comp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers flight-zone path scientific split time
  ];
  testHaskellDepends = [
    aeson base containers flight-zone hlint scientific split time
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Hang gliding and paragliding competition scoring inputs";
  license = stdenv.lib.licenses.bsd3;
}
