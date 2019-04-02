{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, doctest, split, stdenv, time }:
      mkDerivation {
        pname = "flight-clip";
        version = "1.1.0";
        src = ./.;
        libraryHaskellDepends = [ base split time ];
        testHaskellDepends = [ base doctest split time ];
        homepage = "https://github.com/blockscope/flare-timing/tree/master/clip#readme";
        description = "Clipping a pilot's tracklogs";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
