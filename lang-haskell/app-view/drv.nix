{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, ghcjs-base
      , ghcjs-dom, mtl, reflex, reflex-dom, scientific, stdenv, text
      , time
      }:
      mkDerivation {
        pname = "app-view";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base containers ghcjs-base ghcjs-dom mtl reflex reflex-dom
          scientific text time
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
