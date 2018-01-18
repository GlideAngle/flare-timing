{ mkDerivation, base, cmdargs, directory, filemanip, filepath
, flight-span, hlint, mtl, raw-strings-qq, stdenv, system-filepath
, transformers
}:
mkDerivation {
  pname = "flight-cmd";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base cmdargs directory filemanip filepath flight-span mtl
    raw-strings-qq system-filepath transformers
  ];
  testHaskellDepends = [
    base cmdargs directory filemanip filepath flight-span hlint mtl
    raw-strings-qq system-filepath transformers
  ];
  homepage = "https://github.com/BlockScope/flare-timing#readme";
  description = "Command line options";
  license = stdenv.lib.licenses.bsd3;
}
