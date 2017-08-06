{ mkDerivation, aeson, base, containers, hlint, path, scientific
, split, stdenv
}:
mkDerivation {
  pname = "flight-comp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers path scientific split
  ];
  testHaskellDepends = [
    aeson base containers hlint scientific split
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-comp#readme";
  description = "Hang gliding and paragliding competition scoring inputs";
  license = stdenv.lib.licenses.bsd3;
}
