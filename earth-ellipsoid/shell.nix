{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via-sci, base, bifunctors, fgl
      , flight-latlng, flight-units, flight-zone, hcoord, hlint, mtl
      , numbers, scientific, siggy-chardust, stdenv, tasty-compare
      , uom-plugin
      }:
      mkDerivation {
        pname = "flight-earth-ellipsoid";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
          flight-zone hcoord mtl numbers scientific siggy-chardust uom-plugin
        ];
        testHaskellDepends = [
          aeson aeson-via-sci base bifunctors fgl flight-latlng flight-units
          flight-zone hcoord hlint mtl numbers scientific siggy-chardust
          tasty-compare uom-plugin
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
        description = "Distances on the FAI sphere";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
