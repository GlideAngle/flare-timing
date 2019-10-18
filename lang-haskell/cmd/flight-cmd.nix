{ mkDerivation, base, cmdargs, directory, filemanip, filepath
, flight-span, mtl, raw-strings-qq, stdenv, transformers
}:
mkDerivation {
  pname = "flight-cmd";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base cmdargs directory filemanip filepath flight-span mtl
    raw-strings-qq transformers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "Command line options";
  license = stdenv.lib.licenses.mpl20;
}
