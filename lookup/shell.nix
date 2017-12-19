{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via, base, bytestring, cassava
      , containers, directory, filemanip, filepath, flight-comp
      , flight-gap, flight-kml, flight-latlng, flight-mask, flight-zone
      , hcoord, hlint, lens, mtl, path, scientific, split, stdenv, time
      , unordered-containers, uom-plugin
      }:
      mkDerivation {
        pname = "flight-lookup";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via base bytestring cassava containers directory
          filemanip filepath flight-comp flight-gap flight-kml flight-latlng
          flight-mask flight-zone hcoord lens mtl path scientific split time
          unordered-containers uom-plugin
        ];
        testHaskellDepends = [
          aeson aeson-via base bytestring cassava containers directory
          filemanip filepath flight-comp flight-gap flight-kml flight-latlng
          flight-mask flight-zone hcoord hlint lens mtl path scientific split
          time unordered-containers uom-plugin
        ];
        homepage = "https://github.com/BlockScope/flare-timing#readme";
        description = "Hang gliding and paragliding competition data access";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
