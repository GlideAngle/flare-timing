{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
./siggy-chardust.nix
{ }
