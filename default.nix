{ compiler ? "ghc822"
}:

let
  config = import ./nix/config.nix {};
  pkgs = import ./nix/nixpkgs.nix { inherit config; };
  hp = pkgs.haskellPackages;

in
  { detour-via-sci = hp.detour-via-sci;
    detour-via-uom = hp.detour-via-uom;
    flare-timing = hp.flare-timing;
    flight-cmd = hp.flight-cmd;
    flight-comp = hp.flight-comp;
    flight-earth = hp.flight-earth;
    flight-fsdb = hp.flight-fsdb;
    flight-gap = hp.flight-gap;
    flight-igc = hp.flight-igc;
    flight-kml = hp.flight-kml;
    flight-latlng = hp.flight-latlng;
    flight-lookup = hp.flight-lookup;
    flight-mask = hp.flight-mask;
    flight-route = hp.flight-route;
    flight-scribe = hp.flight-scribe;
    flight-span = hp.flight-span;
    flight-task = hp.flight-task;
    flight-track = hp.flight-track;
    flight-units = hp.flight-units;
    flight-zone = hp.flight-zone;
    siggy-chardust = hp.siggy-chardust;
    tasty-compare = hp.tasty-compare;
  }
