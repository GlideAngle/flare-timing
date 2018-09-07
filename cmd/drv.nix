{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cmdargs, directory, filemanip, filepath
      , flight-span, mtl, raw-strings-qq, stdenv, system-filepath
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
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Command line options";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
