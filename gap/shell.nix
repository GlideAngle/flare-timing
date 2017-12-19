{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-via, base, containers, hlint
      , newtype, smallcheck, statistics, stdenv, tasty, tasty-hunit
      , tasty-quickcheck, tasty-smallcheck, vector
      }:
      mkDerivation {
        pname = "flight-gap";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson aeson-via base containers newtype statistics vector
        ];
        testHaskellDepends = [
          aeson aeson-via base containers hlint newtype smallcheck statistics
          tasty tasty-hunit tasty-quickcheck tasty-smallcheck vector
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-gap#readme";
        description = "GAP Scoring";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
