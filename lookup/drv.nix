{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cassava, containers
      , detour-via-sci, directory, filemanip, filepath, flight-clip
      , flight-comp, flight-gap, flight-kml, flight-latlng, flight-mask
      , flight-route, flight-zone, lens, mtl, path, scientific, split
      , stdenv, time, unordered-containers, uom-plugin
      }:
      mkDerivation {
        pname = "flight-lookup";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring cassava containers detour-via-sci directory
          filemanip filepath flight-clip flight-comp flight-gap flight-kml
          flight-latlng flight-mask flight-route flight-zone lens mtl path
          scientific split time unordered-containers uom-plugin
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Hang gliding and paragliding competition data access";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
