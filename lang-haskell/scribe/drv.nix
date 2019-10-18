{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cassava, containers
      , detour-via-sci, directory, filemanip, filepath, flight-clip
      , flight-comp, flight-gap, flight-latlng, flight-route, flight-zone
      , mtl, path, safe-exceptions, scientific, split, stdenv, text, time
      , unordered-containers, vector, yaml
      }:
      mkDerivation {
        pname = "flight-scribe";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring cassava containers detour-via-sci directory
          filemanip filepath flight-clip flight-comp flight-gap flight-latlng
          flight-route flight-zone mtl path safe-exceptions scientific split
          text time unordered-containers vector yaml
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Hang gliding and paragliding competition scoring files";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
