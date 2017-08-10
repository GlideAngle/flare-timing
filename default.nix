{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", flight-comp, flight-kml }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flight-track.nix {
  flight-comp = flight-comp;
  flight-kml = flight-kml;
})
