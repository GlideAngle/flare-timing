{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, call-stack, hlint, stdenv, tasty
      , tasty-hunit
      }:
      mkDerivation {
        pname = "tasty-compare";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base call-stack tasty tasty-hunit ];
        testHaskellDepends = [ base call-stack hlint tasty tasty-hunit ];
        homepage = "https://github.com/BlockScope/tasty-compare#readme";
        description = "Tasty HUnit extensions for comparisons";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
