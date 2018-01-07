{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via-sci, base, containers
      , flight-comp, flight-latlng, flight-zone, hlint, hxt, hxt-xpath
      , parsec, path, raw-strings-qq, scientific, smallcheck, split
      , stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
      , time
      }:
      mkDerivation {
        pname = "flight-fsdb";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via-sci base containers flight-comp flight-latlng
          flight-zone hxt hxt-xpath parsec path scientific split time
        ];
        testHaskellDepends = [
          aeson aeson-via-sci base containers flight-comp flight-latlng
          flight-zone hlint hxt hxt-xpath parsec path raw-strings-qq
          scientific smallcheck split tasty tasty-hunit tasty-quickcheck
          tasty-smallcheck time
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-fsdb#readme";
        description = "A parser for fsdb, the database XML format of FS";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
