{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hlint, smallcheck, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, tasty-smallcheck
      }:
      mkDerivation {
        pname = "siggy-chardust";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base ];
        testHaskellDepends = [
          base hlint smallcheck tasty tasty-hunit tasty-quickcheck
          tasty-smallcheck
        ];
        homepage = "https://github.com/BlockScope/haskell-siggy-chardust#readme";
        description = "Rounding keeping decimal places and significant digits";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
