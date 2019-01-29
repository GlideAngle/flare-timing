{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, cassava, doctest, newtype
      , scientific, siggy-chardust, stdenv, template-haskell
      }:
      mkDerivation {
        pname = "detour-via-sci";
        version = "1.0.1";
        src = ./.;
        libraryHaskellDepends = [
          aeson base cassava newtype scientific siggy-chardust
          template-haskell
        ];
        testHaskellDepends = [
          aeson base cassava doctest newtype scientific siggy-chardust
          template-haskell
        ];
        homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-sci#readme";
        description = "JSON and CSV encoding for rationals as decimal point numbers";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
