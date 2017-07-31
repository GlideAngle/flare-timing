{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", fgl, siggy-chardust }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(hlib.dontHaddock(callPackage ./flight-task.nix {
  fgl = fgl;
  siggy-chardust = siggy-chardust;
}))
