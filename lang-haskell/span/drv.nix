{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cmdargs, stdenv }:
      mkDerivation {
        pname = "flight-span";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base cmdargs ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "How to measure a distance that spans two points";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
