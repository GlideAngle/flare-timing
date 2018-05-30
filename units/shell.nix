{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, hlint, numbers
      , siggy-chardust, stdenv, uom-plugin
      }:
      mkDerivation {
        pname = "flight-units";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base bifunctors numbers siggy-chardust uom-plugin
        ];
        testHaskellDepends = [
          base bifunctors hlint numbers siggy-chardust uom-plugin
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-task#readme";
        description = "Units used in hang gliding and paragliding competitions";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
