{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, detour-via-sci
      , doctest, flight-comp, flight-gap, flight-latlng, flight-units
      , flight-zone, hxt, hxt-pickle-utils, hxt-xpath, megaparsec
      , newtype, path, scientific, split, statistics, stdenv, time
      , uom-plugin, vector
      }:
      mkDerivation {
        pname = "flight-fsdb";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers detour-via-sci flight-comp flight-gap
          flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
          newtype path scientific split statistics time uom-plugin vector
        ];
        testHaskellDepends = [
          aeson base containers detour-via-sci doctest flight-comp flight-gap
          flight-latlng flight-units flight-zone hxt hxt-pickle-utils
          hxt-xpath megaparsec newtype path scientific split statistics time
          uom-plugin vector
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "A parser for fsdb, the database XML format of FS";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
