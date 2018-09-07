{ mkDerivation, base, ghc, stdenv }:
mkDerivation {
  pname = "ghc-tcplugins-extra";
  version = "0.2.5";
  sha256 = "9af31e6a80bf6e4470b13d59fba2c45e23938af937930c534b3b36911b117876";
  libraryHaskellDepends = [ base ghc ];
  homepage = "http://github.com/clash-lang/ghc-tcplugins-extra";
  description = "Utilities for writing GHC type-checker plugins";
  license = stdenv.lib.licenses.bsd2;
}
