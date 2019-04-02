{ compiler ? "ghc822"
}:

# NOTE: For adding overrides without nuking previous overrides with
# overrides = lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
# SEE: https://github.com/NixOS/nixpkgs/issues/26561
# NOTE: Use super for super.callPackage but self for self.package.
# self: Fix-point result.
# super: Result of the composition beforehand.
# SEE: https://nbp.github.io/slides/NixCon/2017.NixpkgsOverlays/
{
  allowUnsupportedSystem = true;
  allowUnfree = true;

  packageOverrides = pkgs:
    let old = pkgs.haskell.packages.${compiler}; in rec {
    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
      {
        app-serve =
          super.callPackage ../app-serve/app-serve.nix
            { siggy-chardust = self.siggy-chardust;
              flight-cmd = self.flight-cmd;
              flight-comp = self.flight-comp;
              flight-gap = self.flight-gap;
              flight-kml = self.flight-kml;
              flight-latlng = self.flight-latlng;
              flight-mask = self.flight-mask;
              flight-route = self.flight-route;
              flight-scribe = self.flight-scribe;
            };

        detour-via-sci =
          super.callPackage ../detour-via-sci/detour-via-sci.nix
            { siggy-chardust = self.siggy-chardust; };

        detour-via-uom =
          super.callPackage ../detour-via-uom/detour-via-uom.nix
            { detour-via-sci = self.detour-via-sci; };

        flare-timing =
          super.callPackage ../flare-timing/flare-timing.nix
            { flight-clip = self.flight-clip;
              flight-cmd = self.flight-cmd;
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
              flight-time = self.flight-time;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
            };

        flight-clip = super.callPackage ../clip/flight-clip.nix {};

        flight-cmd = super.callPackage ../cmd/flight-cmd.nix
              { flight-span = self.flight-span; };

        flight-comp =
          super.callPackage ../comp/flight-comp.nix
            { detour-via-sci = self.detour-via-sci;
              flight-clip = self.flight-clip;
              flight-earth = self.flight-earth;
              flight-gap = self.flight-gap;
              flight-latlng = self.flight-latlng;
              flight-route = self.flight-route;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
            };

        flight-earth =
          super.callPackage ../earth/flight-earth.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              detour-via-uom = self.detour-via-sci;
              flight-latlng = self.flight-latlng;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
              tasty-compare = self.tasty-compare;
              hcoord = self.hcoord;
              hcoord-utm = self.hcoord-utm;
            };

        flight-fsdb =
          super.callPackage ../fsdb/flight-fsdb.nix
            { detour-via-sci = self.detour-via-sci;
              flight-comp = self.flight-comp;
              flight-gap = self.flight-gap;
              flight-latlng = self.flight-latlng;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
            };

        flight-gap =
          super.callPackage ../gap/flight-gap.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              detour-via-uom = self.detour-via-uom;
              flight-units = self.flight-units;
            };

        flight-igc =
          super.callPackage ../igc/flight-igc.nix
            { flight-clip = self.flight-clip; };


        flight-kml =
          super.callPackage ../kml/flight-kml.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              flight-clip = self.flight-clip;
            };

        flight-latlng =
          super.callPackage ../latlng/flight-latlng.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              detour-via-uom = self.detour-via-uom;
              flight-units = self.flight-units;
            };

        flight-lookup =
          super.callPackage ../lookup/flight-lookup.nix
            { detour-via-sci = self.detour-via-sci;
              flight-clip = self.flight-clip;
              flight-comp = self.flight-comp;
              flight-gap = self.flight-gap;
              flight-kml = self.flight-kml;
              flight-latlng = self.flight-latlng;
              flight-mask = self.flight-mask;
              flight-route = self.flight-route;
              flight-zone = self.flight-zone;
            };

        flight-mask =
          super.callPackage ../mask/flight-mask.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              flight-clip = self.flight-clip;
              flight-comp = self.flight-comp;
              flight-earth = self.flight-earth;
              flight-gap = self.flight-gap;
              flight-kml = self.flight-kml;
              flight-latlng = self.flight-latlng;
              flight-route = self.flight-route;
              flight-scribe = self.flight-scribe;
              flight-span = self.flight-span;
              flight-task = self.flight-task;
              flight-track = self.flight-track;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
            };

        flight-route =
          super.callPackage ../route/flight-route.nix
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
          super.callPackage ../scribe/flight-scribe.nix
            { detour-via-sci = self.detour-via-sci;
              flight-clip = self.flight-clip;
              flight-comp = self.flight-comp;
              flight-gap = self.flight-gap;
              flight-latlng = self.flight-latlng;
              flight-route = self.flight-route;
              flight-zone = self.flight-zone;
            };

        flight-span = super.callPackage ../span/flight-span.nix {};

        flight-task =
          super.callPackage ../task/flight-task.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              flight-earth = self.flight-earth;
              flight-latlng = self.flight-latlng;
              flight-units = self.flight-units;
              flight-zone = self.flight-zone;
              tasty-compare = self.flight-tasty-compare;
            };

        flight-time =
          super.callPackage ../time/flight-time.nix
            { siggy-chardust = self.siggy-chardust;
              flight-clip = self.flight-clip;
              flight-comp = self.flight-comp;
              flight-kml = self.flight-kml;
              flight-latlng = self.flight-latlng;
              flight-lookup = self.flight-lookup;
              flight-mask = self.flight-mask;
              flight-scribe = self.flight-scribe;
            };

        flight-track =
          super.callPackage ../track/flight-track.nix
            { flight-clip = self.flight-clip;
              flight-comp = self.flight-comp;
              flight-kml = self.flight-kml;
              flight-igc = self.flight-igc;
            };

        flight-units =
          super.callPackage ../units/flight-units.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
            };

        flight-zone =
          super.callPackage ../zone/flight-zone.nix
            { siggy-chardust = self.siggy-chardust;
              detour-via-sci = self.detour-via-sci;
              detour-via-uom = self.detour-via-uom;
              flight-units = self.flight-units;
              flight-latlng = self.flight-latlng;
            };

        megaparsec = super.callPackage ./megaparsec.nix {};
        hcoord = super.callPackage ./hcoord.nix {};
        hcoord-utm = super.callPackage ./hcoord-utm.nix {};
        siggy-chardust = super.callPackage ../siggy-chardust/siggy-chardust.nix {};
        tasty-compare = super.callPackage ../tasty-compare/tasty-compare.nix {};
      });
    };
  };
}
