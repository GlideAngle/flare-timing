{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hlint, parsec, stdenv }:
      mkDerivation {
        pname = "flight-igc";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base parsec ];
        testHaskellDepends = [ base hlint ];
        homepage = "https://github.com/BlockScope/haskell-flight-igc#readme";
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
