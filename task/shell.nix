{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via, base, bifunctors, fgl
      , flight-latlng, flight-units, flight-zone, hcoord, hlint, mtl
      , numbers, scientific, siggy-chardust, smallcheck, stdenv, tasty
      , tasty-compare, tasty-hunit, tasty-quickcheck, tasty-smallcheck
      , uom-plugin
      }:
      mkDerivation {
        pname = "flight-task";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via base bifunctors fgl flight-latlng flight-units
          flight-zone hcoord mtl numbers scientific siggy-chardust uom-plugin
        ];
        testHaskellDepends = [
          aeson aeson-via base bifunctors fgl flight-latlng flight-units
          flight-zone hcoord hlint mtl numbers scientific siggy-chardust
          smallcheck tasty tasty-compare tasty-hunit tasty-quickcheck
          tasty-smallcheck uom-plugin
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
        description = "Tasks to fly";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
