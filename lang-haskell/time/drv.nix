{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, Cabal, Diff, directory, filepath
      , flight-clip, flight-comp, flight-kml, flight-latlng
      , flight-lookup, flight-mask, flight-scribe, lens, microlens, mtl
      , prettyprinter, safe-exceptions, siggy-chardust, stdenv, tasty
      , tasty-golden, text, these, time, transformers, uom-plugin
      , utf8-string, vector
      }:
      mkDerivation {
        pname = "flight-time";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base directory filepath flight-clip flight-comp flight-kml
          flight-latlng flight-lookup flight-mask flight-scribe lens mtl
          safe-exceptions siggy-chardust these time uom-plugin
        ];
        testHaskellDepends = [
          aeson base Cabal Diff directory filepath flight-clip flight-comp
          flight-kml flight-latlng flight-lookup flight-mask flight-scribe
          lens microlens mtl prettyprinter safe-exceptions siggy-chardust
          tasty tasty-golden text these time transformers uom-plugin
          utf8-string vector
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Align times of competing pilot's tracklogs";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
