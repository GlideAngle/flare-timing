{ compiler ? "ghc822" }:

let
  # SEE: https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pkgs = let
     hostPkgs = import <nixpkgs> {};
     pinnedVersion = hostPkgs.lib.importJSON ./nix/nixpkgs-version.json;
     pinnedPkgs = hostPkgs.fetchgit {
       inherit (pinnedVersion) url rev sha256;
     };
  in import pinnedPkgs {config = { allowUnsupportedSystem = true; allowUnfree = true; }; };

  cp = pkgs.haskell.packages.${compiler}.callPackage;
in
  cp ./siggy-chardust/siggy-chardust.nix { }
