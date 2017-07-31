{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", fgl, siggy-chardust }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
# NOTE: Add dontHaddock to workaround https://github.com/haskell/haddock/issues/233
hlib.dontCheck(hlib.dontHaddock(callPackage ./flight-task.nix {
  fgl = fgl;
  siggy-chardust = siggy-chardust;
}))
