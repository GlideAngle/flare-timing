{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, doctest, megaparsec, stdenv
      , utf8-string
      }:
      mkDerivation {
        pname = "flight-igc";
        version = "2.0.0";
        src = ./.;
        libraryHaskellDepends = [ base bytestring megaparsec utf8-string ];
        testHaskellDepends = [
          base bytestring doctest megaparsec utf8-string
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "A parser for IGC files";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
