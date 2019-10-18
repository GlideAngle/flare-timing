{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cmdargs, directory
      , filemanip, filepath, flight-clip, flight-cmd, flight-comp
      , flight-gap, flight-kml, flight-latlng, flight-mask, flight-route
      , flight-scribe, hlint, mtl, raw-strings-qq, safe-exceptions
      , servant, servant-server, siggy-chardust, stdenv, time
      , transformers, uom-plugin, wai, wai-cors, wai-extra, warp, yaml
      }:
      mkDerivation {
        pname = "app-serve";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring cmdargs directory filemanip filepath
          flight-clip flight-cmd flight-comp flight-gap flight-kml
          flight-latlng flight-mask flight-route flight-scribe mtl
          raw-strings-qq safe-exceptions servant servant-server
          siggy-chardust time transformers uom-plugin wai wai-cors wai-extra
          warp yaml
        ];
        testHaskellDepends = [ base flight-comp hlint ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
