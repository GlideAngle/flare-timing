{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, cassava, hlint, newtype
      , scientific, stdenv, template-haskell
      }:
      mkDerivation {
        pname = "aeson-via-sci";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base cassava newtype scientific template-haskell
        ];
        testHaskellDepends = [
          aeson base cassava hlint newtype scientific template-haskell
        ];
        homepage = "https://github.com/BlockScope/flare-timing#readme";
        description = "JSON encoding and decoding for rationals via scientific";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
