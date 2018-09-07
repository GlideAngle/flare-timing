{ mkDerivation, base, cmdargs, stdenv }:
mkDerivation {
  pname = "flight-span";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base cmdargs ];
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "How to measure a distance that spans two points";
  license = stdenv.lib.licenses.mpl20;
}
