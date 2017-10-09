{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", siggy-chardust }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
# NOTE: Add dontHaddockVj to workaround https://github.com/haskell/haddock/issues/233
hlib.dontHaddock(callPackage ./flight-latlng.nix {
  siggy-chardust = siggy-chardust;
})
