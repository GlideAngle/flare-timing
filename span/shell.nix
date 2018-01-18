{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cmdargs, hlint, stdenv }:
      mkDerivation {
        pname = "flight-span";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [ base cmdargs ];
        testHaskellDepends = [ base cmdargs hlint ];
        homepage = "https://github.com/BlockScope/haskell-flight-mask#readme";
        description = "How to measure a distance that spans two points";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
