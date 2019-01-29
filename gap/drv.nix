{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, detour-via-sci
      , detour-via-uom, flight-units, newtype, scientific, siggy-chardust
      , smallcheck, statistics, stdenv, tasty, tasty-hunit
      , tasty-quickcheck, tasty-smallcheck, template-haskell, text
      , uom-plugin, vector
      }:
      mkDerivation {
        pname = "flight-gap";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers detour-via-sci detour-via-uom flight-units
          newtype scientific siggy-chardust statistics template-haskell text
          uom-plugin vector
        ];
        testHaskellDepends = [
          aeson base containers detour-via-sci detour-via-uom flight-units
          newtype scientific siggy-chardust smallcheck statistics tasty
          tasty-hunit tasty-quickcheck tasty-smallcheck template-haskell text
          uom-plugin vector
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "GAP Scoring";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
