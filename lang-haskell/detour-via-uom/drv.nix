{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, cassava, detour-via-sci, doctest
      , newtype, scientific, stdenv, uom-plugin
      }:
      mkDerivation {
        pname = "detour-via-uom";
        version = "1.0.1";
        src = ./.;
        libraryHaskellDepends = [
          aeson base cassava detour-via-sci newtype scientific uom-plugin
        ];
        testHaskellDepends = [
          aeson base cassava detour-via-sci doctest newtype scientific
          uom-plugin
        ];
        homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-uom#readme";
        description = "JSON and CSV encoding for quantities";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
