{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", flight-fsdb, flight-kml, flight-igc }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flare-timing.nix {
  flight-fsdb = flight-fsdb;
  flight-kml = flight-kml ;
  flight-igc = flight-igc ;
})
