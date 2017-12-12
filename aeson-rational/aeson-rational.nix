{ mkDerivation, base, bifunctors, hlint, numbers
, stdenv
}:
mkDerivation {
  pname = "aeson-rational";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors numbers
  ];
  testHaskellDepends = [
    base bifunctors hlint numbers
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Units used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.bsd3;
}
