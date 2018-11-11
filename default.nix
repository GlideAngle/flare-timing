{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: with pkgs.lib; {
  packages = {
    # WARNING: Until I get to choose a version of ghc later than the default of
    # ghc-8.0.2, comment out the packages I might need with ghc.
    # 
    # detour-via-sci = ./detour-via-sci;
    # detour-via-uom = ./detour-via-uom;
    # cmd = ./cmd;
    # comp = ./comp;
    # earth = ./earth;
    # fsdb = ./fsdb;
    # gap = ./gap;
    # igc = ./igc;
    # kml = ./kml;
    # latlng = ./latlng;
    # lookup = ./lookup;
    # mask = ./mask;
    # route = ./route;
    # scribe = ./scribe;
    # siggy-chardust = ./siggy-chardust;
    # span = ./span;
    # task = ./task;
    # tasty-compare = ./tasty-compare;
    # track = ./track;
    # units = ./units;
    # zone = ./zone;
    # hcoord = .stack2cabal/hcoord/hcoord;
    # hcoord-utm = .stack2cabal/hcoord/hcoord-utm;
    app-view = ./app-view;
  };

  overrides = self: super:
    {
      detour-via-sci =
        super.callPackage ./detour-via-sci/detour-via-sci.nix
          { siggy-chardust = self.siggy-chardust; };

      detour-via-uom =
        super.callPackage ./detour-via-uom/detour-via-uom.nix
          { detour-via-sci = self.detour-via-sci; };

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

      flight-cmd = super.callPackage ./cmd/flight-cmd.nix
            { flight-span = self.flight-span; };

      flight-comp =
        super.callPackage ./comp/flight-comp.nix
          { detour-via-sci = self.detour-via-sci;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-route = self.flight-route;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
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

      flight-fsdb =
        super.callPackage ./fsdb/flight-fsdb.nix
          { detour-via-sci = self.detour-via-sci;
            flight-comp = self.flight-comp;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-units = self.flight-units;
            flight-zone = self.flight-zone;
          };

      flight-gap =
        super.callPackage ./gap/flight-gap.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
          };

      flight-igc = super.callPackage ./igc/flight-igc.nix {};

      flight-kml =
        super.callPackage ./kml/flight-kml.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
          };

      flight-latlng =
        super.callPackage ./latlng/flight-latlng.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
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

      flight-scribe =
        super.callPackage ./scribe/flight-scribe.nix
          { detour-via-sci = self.detour-via-sci;
            flight-comp = self.flight-comp;
            flight-latlng = self.flight-latlng;
            flight-gap = self.flight-gap;
            flight-route = self.flight-route;
            flight-zone = self.flight-zone;
          };

      flight-span = super.callPackage ./span/flight-span.nix {};

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

      flight-track =
        super.callPackage ./track/flight-track.nix
          { flight-comp = self.flight-comp;
            flight-kml = self.flight-kml;
            flight-igc = self.flight-igc;
          };

      flight-units =
        super.callPackage ./units/flight-units.nix
          { siggy-chardust = self.siggy-chardust; };

      flight-zone =
        super.callPackage ./zone/flight-zone.nix
          { siggy-chardust = self.siggy-chardust;
            detour-via-sci = self.detour-via-sci;
            detour-via-uom = self.detour-via-uom;
            flight-units = self.flight-units;
            flight-latlng = self.flight-latlng;
          };

      hcoord = super.callPackage ./nix/hcoord.nix {};
      hcoord-utm = super.callPackage ./nix/hcoord-utm.nix {};
      siggy-chardust = super.callPackage ./siggy-chardust/siggy-chardust.nix {};
      tasty-compare = super.callPackage ./tasty-compare/tasty-compare.nix {};
    };

  shells = {
    ghc = [
      # "detour-via-sci"
      # "detour-via-uom"
      # "cmd"
      # "comp"
      # "earth"
      # "fsdb"
      # "gap"
      # "igc"
      # "kml"
      # "latlng"
      # "lookup"
      # "mask"
      # "route"
      # "scribe"
      # "siggy-chardust"
      # "span"
      # "task"
      # "tasty-compare"
      # "track"
      # "units"
      # "zone"
      # "hcoord"
      # "hcoord-utm"
      # "text"
      "app-view"
    ];
    ghcjs = ["app-view"];
  };
})
