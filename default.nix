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

  detour-via-sci =
    hp.callPackage ./detour-via-sci/detour-via-sci.nix
      { siggy-chardust = siggy-chardust; };

  detour-via-uom =
    hp.callPackage ./detour-via-uom/detour-via-uom.nix
      { detour-via-sci = detour-via-sci; };

  flight-units =
    hp.callPackage ./units/flight-units.nix
      { siggy-chardust = siggy-chardust; };

  flight-latlng =
    hp.callPackage ./latlng/flight-latlng.nix
      { siggy-chardust = siggy-chardust;
        detour-via-sci = detour-via-sci;
        detour-via-uom = detour-via-uom;
        flight-units = flight-units;
      };

in
  { siggy-chardust = siggy-chardust;
    detour-via-sci = detour-via-sci;
    detour-via-uom = detour-via-uom;
    flight-units = flight-units;
    flight-latlng = flight-latlng;
  }
