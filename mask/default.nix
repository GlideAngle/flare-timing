{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802", siggy-chardust, flight-units, flight-comp, flight-kml, flight-gap, flight-task, flight-track }:

with nixpkgs.pkgs.haskell.packages.${compiler};

let hlib = nixpkgs.pkgs.haskell.lib;
in
hlib.dontCheck(callPackage ./flight-mask.nix {
  siggy-chardust = siggy-chardust;
  flight-units = flight-units;
  flight-comp = flight-comp;
  flight-kml = flight-kml;
  flight-gap = flight-gap;
  flight-task= flight-task;
  flight-track = flight-track;
})
