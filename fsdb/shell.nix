{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, detour-via-sci
      , flight-comp, flight-gap, flight-latlng, flight-units, flight-zone
      , hxt, hxt-xpath, megaparsec, newtype, path, scientific, smallcheck
      , split, stdenv, tasty, tasty-hunit, tasty-quickcheck
      , tasty-smallcheck, time, uom-plugin
      }:
      mkDerivation {
        pname = "flight-fsdb";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers detour-via-sci flight-comp flight-gap
          flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
          newtype path scientific split time uom-plugin
        ];
        testHaskellDepends = [
          aeson base containers detour-via-sci flight-comp flight-gap
          flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
          newtype path scientific smallcheck split tasty tasty-hunit
          tasty-quickcheck tasty-smallcheck time uom-plugin
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
