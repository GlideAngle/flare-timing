{ pkgs ? import ../nix/nixpkgs.nix {}
, compiler ? "ghc822"
}:

let
  hlib = pkgs.haskell.lib;

  fgl-src = pkgs.fetchgit {
          url = "http://github.com/haskell/fgl.git";
          sha256 = "0biwsaj6s24l8ix95hkw4syl87ywxy363pr413kazzbhi0csf20s";
          rev = "e29503775e474b7a7cd8951b6bebcf1529b888b5";
        };

  hcoord-src = pkgs.fetchgit {
          url = "http://github.com/BlockScope/hcoord.git";
          rev = "bb17ddba271829f3902d9ae3af97f2723eb0ab47";
        };

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      fgl = super.callCabal2nix fgl-src {};
      hcoord = super.callCabal2nix (hcoord-src + "/hcoord") {};
      hcoord-utm = super.callCabal2nix (hcoord-src + "/hcoord-utm") {};
    };
  };

in hp.callPackage ./siggy-chardust.nix {}
