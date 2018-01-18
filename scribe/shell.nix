{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via-sci, base, bytestring
      , cassava, containers, directory, filemanip, filepath, flight-comp
      , flight-gap, flight-latlng, flight-route, flight-zone, hlint, mtl
      , path, scientific, split, stdenv, time, unordered-containers
      , vector, yaml
      }:
      mkDerivation {
        pname = "flight-scribe";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via-sci base bytestring cassava containers directory
          filemanip filepath flight-comp flight-gap flight-latlng
          flight-route flight-zone mtl path scientific split time
          unordered-containers vector yaml
        ];
        testHaskellDepends = [
          aeson aeson-via-sci base bytestring cassava containers directory
          filemanip filepath flight-comp flight-gap flight-latlng
          flight-route flight-zone hlint mtl path scientific split time
          unordered-containers vector yaml
        ];
        homepage = "https://github.com/BlockScope/flare-timing#readme";
        description = "Hang gliding and paragliding competition scoring files";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
