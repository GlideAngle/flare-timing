{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
# NOTE: Add dontHaddock to workaround https://github.com/haskell/haddock/issues/233
hlib.dontHaddock(callPackage ./aeson-rational.nix {
})
