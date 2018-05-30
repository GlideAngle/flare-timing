{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via-sci, base, hlint, hxt
      , hxt-xpath, parsec, path, raw-strings-qq, siggy-chardust
      , smallcheck, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
      , tasty-smallcheck, time
      }:
      mkDerivation {
        pname = "flight-kml";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via-sci base hxt hxt-xpath parsec path siggy-chardust
          split time
        ];
        testHaskellDepends = [
          aeson aeson-via-sci base hlint hxt hxt-xpath parsec path
          raw-strings-qq siggy-chardust smallcheck split tasty tasty-hunit
          tasty-quickcheck tasty-smallcheck time
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-kml#readme";
        description = "A parser for KML files";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
