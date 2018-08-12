{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, doctest, smallcheck, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, tasty-smallcheck
      }:
      mkDerivation {
        pname = "siggy-chardust";
        version = "1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base ];
        testHaskellDepends = [
          base doctest smallcheck tasty tasty-hunit tasty-quickcheck
          tasty-smallcheck
        ];
        homepage = "https://github.com/blockscope/flare-timing/tree/master/siggy-chardust#readme";
        description = "Rounding rationals to significant digits and decimal places";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
