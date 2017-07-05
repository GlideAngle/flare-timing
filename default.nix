{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flight-gap.nix {})
