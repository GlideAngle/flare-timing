{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via-sci, base, bifunctors
      , flight-earth-flat, flight-earth-sphere, flight-latlng
      , flight-task, flight-units, flight-zone, hcoord, hlint, numbers
      , scientific, siggy-chardust, stdenv, uom-plugin
      }:
      mkDerivation {
        pname = "flight-route";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via-sci base bifunctors flight-earth-flat
          flight-earth-sphere flight-latlng flight-task flight-units
          flight-zone hcoord numbers scientific siggy-chardust uom-plugin
        ];
        testHaskellDepends = [
          aeson aeson-via-sci base bifunctors flight-earth-flat
          flight-earth-sphere flight-latlng flight-task flight-units
          flight-zone hcoord hlint numbers scientific siggy-chardust
          uom-plugin
        ];
        homepage = "https://github.com/BlockScope/flare-timing#readme";
        description = "Control zones to fly";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
