{ mkDerivation, base, containers, deepseq, ghc, ghc-tcplugins-extra
, hlint, stdenv, tasty, tasty-hunit, template-haskell, units-parser
}:
mkDerivation {
  pname = "uom-plugin";
  version = "0.3.0.0";
  sha256 = "94be3fdd1c162afec2c0c16a4ee280308d9c519cf5d061b105d426f211a24699";
  libraryHaskellDepends = [
    base containers deepseq ghc ghc-tcplugins-extra template-haskell
    units-parser
  ];
  testHaskellDepends = [ base hlint tasty tasty-hunit ];
  doCheck = false;
  homepage = "https://github.com/adamgundry/uom-plugin#readme";
  description = "Units of measure as a GHC typechecker plugin";
  license = stdenv.lib.licenses.bsd3;
}
