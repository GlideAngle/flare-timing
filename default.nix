{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", flight-comp, flight-fsdb, flight-kml, flight-igc }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flare-timing.nix {
  flight-comp = flight-comp;
  flight-fsdb = flight-fsdb;
  flight-kml = flight-kml;
  flight-igc = flight-igc;
})
