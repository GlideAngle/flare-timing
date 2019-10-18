{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bifunctors, detour-via-sci
      , detour-via-uom, doctest, fgl, flight-latlng, flight-units
      , flight-zone, hcoord, hcoord-utm, mtl, numbers, scientific
      , siggy-chardust, smallcheck, stdenv, tasty, tasty-compare
      , tasty-hunit, tasty-quickcheck, tasty-smallcheck, uom-plugin
      }:
      mkDerivation {
        pname = "flight-earth";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bifunctors detour-via-sci detour-via-uom fgl
          flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
          numbers scientific siggy-chardust uom-plugin
        ];
        testHaskellDepends = [
          aeson base bifunctors detour-via-sci detour-via-uom doctest fgl
          flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
          numbers scientific siggy-chardust smallcheck tasty tasty-compare
          tasty-hunit tasty-quickcheck tasty-smallcheck uom-plugin
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Distances on the WGS84 ellipsoid, the FAI sphere and the UTM projection";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
