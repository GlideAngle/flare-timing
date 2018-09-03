{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "ghc822"
}:

let
  hlib = nixpkgs.haskell.lib;
  lib = nixpkgs.lib;

  hp = nixpkgs.haskell.packages.${compiler}.override (old: {
    # NOTE: For adding overrides without nuking previous overrides with
    # overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
    # SEE: https://github.com/NixOS/nixpkgs/issues/26561
    # NOTE: Use super for super.callPackage but self for self.package.
    # self: Fix-point result.
    # super: Result of the composition beforehand.
    # SEE: https://nbp.github.io/slides/NixCon/2017.NixpkgsOverlays/
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
    {
      detour-via-sci =
        super.callPackage ./detour-via-sci/detour-via-sci.nix
          { siggy-chardust = self.siggy-chardust; };

      detour-via-uom =
        super.callPackage ./detour-via-uom/detour-via-uom.nix
          { detour-via-sci = self.detour-via-sci; };

      fgl = super.callPackage ./nix/fgl.nix {};
      hcoord = super.callPackage ./nix/hcoord.nix {};
      hcoord-utm = super.callPackage ./nix/hcoord-utm.nix {};

      flight-span = super.callPackage ./span/flight-span.nix {};

      flight-cmd = super.callPackage ./cmd/flight-cmd.nix
            { flight-span = self.flight-span; };

      flight-igc = super.callPackage ./igc/flight-igc.nix {};

      flight-kml =
        super.callPackage ./kml/flight-kml.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
          };

      flight-units =
        super.callPackage ./units/flight-units.nix
          { siggy-chardust = self.siggy-chardust; };

      flight-latlng =
        super.callPackage ./latlng/flight-latlng.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
          };

      flight-zone =
        super.callPackage ./zone/flight-zone.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
            flight-latlng = self.flight-latlng;
          };

      flight-earth =
        super.callPackage ./earth/flight-earth.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            flight-units = self.flight-units;
            flight-latlng = self.flight-latlng;
            flight-zone = self.flight-zone;
            tasty-compare = self.tasty-compare;
            hcoord-utm = self.hcoord-utm;
          };

      flight-gap =
        super.callPackage ./gap/flight-gap.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
          };

      flight-task =
        super.callPackage ./task/flight-task.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            flight-earth = self.flight-earth;
            flight-latlng = self.flight-latlng;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
            tasty-compare = self.flight-tasty-compare;
          };

      flight-route =
        super.callPackage ./route/flight-route.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            flight-earth = self.flight-earth;
            flight-latlng = self.flight-latlng;
            flight-task = self.flight-task;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
            hcoord-utm = self.hcoord-utm;
          };

      flight-comp =
        super.callPackage ./comp/flight-comp.nix
          { detour-via-sci = self.detour-via-sci;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-route = self.flight-route;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
          };

      flight-fsdb =
        super.callPackage ./fsdb/flight-fsdb.nix
          { detour-via-sci = self.detour-via-sci;
            flight-comp = self.flight-comp;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
          };

      flight-scribe =
        super.callPackage ./scribe/flight-scribe.nix
          { detour-via-sci = self.detour-via-sci;
            flight-comp = self.flight-comp;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-route = self.flight-route;
            flight-zone = self.flight-zone;
          };

      flight-track =
        super.callPackage ./track/flight-track.nix
          { flight-comp = self.flight-comp;
            flight-kml = self.flight-kml;
            flight-igc = self.flight-igc;
          };

      flight-mask =
        super.callPackage ./mask/flight-mask.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            flight-span = self.flight-span;
            flight-comp = self.flight-comp;
            flight-earth = self.flight-earth;
            flight-gap = self.flight-gap;
            flight-kml = self.flight-kml;
            flight-latlng = self.flight-latlng;
            flight-route = self.flight-route;
            flight-scribe = self.flight-scribe;
            flight-task = self.flight-task;
            flight-track = self.flight-track;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
          };

      flight-lookup =
        super.callPackage ./lookup/flight-lookup.nix
          { detour-via-sci = self.detour-via-sci;
            flight-comp = self.flight-comp;
            flight-gap = self.flight-gap;
            flight-kml = self.flight-kml;
            flight-latlng = self.flight-latlng;
            flight-mask = self.flight-mask;
            flight-route = self.flight-route;
            flight-zone = self.flight-zone;
          };


      flare-timing =
        super.callPackage ./flare-timing/flare-timing.nix
          { flight-cmd = self.flight-cmd;
            flight-comp = self.flight-comp;
            flight-earth = self.flight-earth;
            flight-fsdb = self.flight-fsdb;
            flight-gap = self.flight-gap;
            flight-igc = self.flight-igc;
            flight-kml = self.flight-kml;
            flight-latlng = self.flight-latlng;
            flight-lookup = self.flight-lookup;
            flight-mask = self.flight-mask;
            flight-route = self.flight-route;
            flight-span = self.flight-span;
            flight-scribe = self.flight-scribe;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
          };

      siggy-chardust = super.callPackage ./siggy-chardust/siggy-chardust.nix {};
      tasty-compare = super.callPackage ./tasty-compare/tasty-compare.nix {};
    });
  });

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
