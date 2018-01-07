{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via, aeson-via-uom, base
      , containers, flight-units, hlint, newtype, scientific
      , siggy-chardust, smallcheck, statistics, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, tasty-smallcheck, uom-plugin
      , vector
      }:
      mkDerivation {
        pname = "flight-gap";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via aeson-via-uom base containers flight-units newtype
          scientific siggy-chardust statistics uom-plugin vector
        ];
        testHaskellDepends = [
          aeson aeson-via aeson-via-uom base containers flight-units hlint
          newtype scientific siggy-chardust smallcheck statistics tasty
          tasty-hunit tasty-quickcheck tasty-smallcheck uom-plugin vector
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-gap#readme";
        description = "GAP Scoring";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
