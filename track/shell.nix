{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, filepath
      , flight-comp, flight-igc, flight-kml, hlint, mtl, path, split
      , stdenv, time
      }:
      mkDerivation {
        pname = "flight-track";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers directory filepath flight-comp flight-igc
          flight-kml mtl path split time
        ];
        testHaskellDepends = [
          base containers directory filepath flight-comp flight-kml hlint mtl
          split time
        ];
        homepage = "https://github.com/BlockScope/haskell-flight-track#readme";
        description = "Hang gliding and paragliding competition track logs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
