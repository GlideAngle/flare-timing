{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "ghc822"
}:

let
  hlib = nixpkgs.haskell.lib;

  fgl-src = nixpkgs.fetchgit {
          url = "http://github.com/haskell/fgl.git";
          sha256 = "0biwsaj6s24l8ix95hkw4syl87ywxy363pr413kazzbhi0csf20s";
          rev = "e29503775e474b7a7cd8951b6bebcf1529b888b5";
        };

  hcoord-src = nixpkgs.fetchgit {
          url = "http://github.com/BlockScope/hcoord.git";
          rev = "bb17ddba271829f3902d9ae3af97f2723eb0ab47";
        };

  hp = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      fgl = super.callCabal2nix fgl-src {};
      hcoord = super.callCabal2nix (hcoord-src + "/hcoord") {};
      hcoord-utm = super.callCabal2nix (hcoord-src + "/hcoord-utm") {};
    };
  };

  siggy-chardust =
    hp.callPackage ./siggy-chardust/siggy-chardust.nix
      {};

  tasty-compare =
    hp.callPackage ./tasty-compare/tasty-compare.nix
      {};

  igc =
    hp.callPackage ./igc/flight-igc.nix
      {};

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

in
  { detour-via-sci = detour-via-sci;
    detour-via-uom = detour-via-uom;
    flight-igc = igc;
    flight-kml = kml;
    flight-latlng = latlng;
    flight-units = units;
    flight-zone = zone;
    siggy-chardust = siggy-chardust;
    tasty-compare = tasty-compare;
  }
