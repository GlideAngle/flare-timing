{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", siggy-chardust, flight-comp, flight-fsdb, flight-kml, flight-igc, flight-gap, flight-task, flight-track }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flare-timing.nix {
  siggy-chardust = siggy-chardust;
  flight-comp = flight-comp;
  flight-fsdb = flight-fsdb;
  flight-kml = flight-kml;
  flight-igc = flight-igc;
  flight-gap = flight-gap;
  flight-task= flight-task;
  flight-track = flight-track;
})
