{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", flight-comp }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flight-fsdb.nix {
  flight-comp = flight-comp;
})
