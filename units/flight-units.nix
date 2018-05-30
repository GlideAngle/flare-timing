{ mkDerivation, base, bifunctors, hlint, numbers, siggy-chardust
, stdenv, uom-plugin
}:
mkDerivation {
  pname = "flight-units";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors numbers siggy-chardust uom-plugin
  ];
  testHaskellDepends = [
    base bifunctors hlint numbers siggy-chardust uom-plugin
  ];
  homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
  description = "Units used in hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
