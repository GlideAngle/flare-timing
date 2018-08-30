{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "ghc822"
}:

let
  hlib = nixpkgs.haskell.lib;
  lib = nixpkgs.lib;

  hp = nixpkgs.haskell.packages.${compiler}.override (old: {
    # SEE: https://github.com/NixOS/nixpkgs/issues/26561
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      Cabal = super.callPackage ./nix/Cabal.nix {};
      hpack = super.callPackage ./nix/hpack.nix {};
      infer-license = super.callPackage ./nix/infer-license.nix {};
      fgl = super.callPackage ./nix/fgl.nix {};
      hcoord = super.callPackage ./nix/hcoord.nix {};
    });
  });

  fgl = hp.callPackage ./nix/fgl.nix {};
  hcoord = hp.callPackage ./nix/hcoord.nix {};
  hcoord-utm = hp.callPackage ./nix/hcoord-utm.nix {};

  siggy-chardust =
    hp.callPackage ./siggy-chardust/siggy-chardust.nix
      {};

  tasty-compare =
    hp.callPackage ./tasty-compare/tasty-compare.nix
      {};

  igc =
    hp.callPackage ./igc/flight-igc.nix
      {};

  span =
    hp.callPackage ./span/flight-span.nix
      {};

  cmd =
    hp.callPackage ./cmd/flight-cmd.nix
      { flight-span = span; };

  detour-via-sci =
    hp.callPackage ./detour-via-sci/detour-via-sci.nix
      { siggy-chardust = siggy-chardust; };

  detour-via-uom =
    hp.callPackage ./detour-via-uom/detour-via-uom.nix
      { detour-via-sci = detour-via-sci; };

  kml =
    hp.callPackage ./kml/flight-kml.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
      };

  units =
    hp.callPackage ./units/flight-units.nix
      { siggy-chardust = siggy-chardust; };

  latlng =
    hp.callPackage ./latlng/flight-latlng.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        detour-via-uom = detour-via-uom;
        flight-units = units;
      };

  zone =
    hp.callPackage ./zone/flight-zone.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        detour-via-uom = detour-via-uom;
        flight-units = units;
        flight-latlng = latlng;
      };

  earth =
    hp.callPackage ./earth/flight-earth.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        flight-units = units;
        flight-latlng = latlng;
        flight-zone = zone;
        tasty-compare = tasty-compare;
        hcoord-utm = hcoord-utm;
      };

  gap =
    hp.callPackage ./gap/flight-gap.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        detour-via-uom = detour-via-uom;
        flight-units = units;
      };

  task =
    hp.callPackage ./task/flight-task.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        flight-earth = earth;
        flight-latlng = latlng;
        flight-units = units;
        flight-zone = zone;
        tasty-compare = tasty-compare;
      };

  route =
    hp.callPackage ./route/flight-route.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        flight-earth = earth;
        flight-latlng = latlng;
        flight-task = task;
        flight-units = units;
        flight-zone = zone;
        hcoord-utm = hcoord-utm;
      };

  comp =
    hp.callPackage ./comp/flight-comp.nix
      { detour-via-sci = detour-via-sci;
        flight-latlng = latlng;
        flight-gap = gap;
        flight-route = route;
        flight-units = units;
        flight-zone = zone;
      };

  fsdb =
    hp.callPackage ./fsdb/flight-fsdb.nix
      { detour-via-sci = detour-via-sci;
        flight-comp = comp;
        flight-latlng = latlng;
        flight-gap = gap;
        flight-units = units;
        flight-zone = zone;
      };

in
  { detour-via-sci = detour-via-sci;
    detour-via-uom = detour-via-uom;
    flight-cmd = cmd;
    flight-comp = comp;
    flight-earth = earth;
    flight-fsdb = fsdb;
    flight-gap = gap;
    flight-igc = igc;
    flight-kml = kml;
    flight-latlng = latlng;
    flight-route = route;
    flight-span = span;
    flight-task = task;
    flight-units = units;
    flight-zone = zone;
    siggy-chardust = siggy-chardust;
    tasty-compare = tasty-compare;
  }
