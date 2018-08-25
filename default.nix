{ compiler ? "ghc822" }:

let
  # SEE: https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pkgs = let
     hostPkgs = import <nixpkgs> {};
     pinnedVersion = hostPkgs.lib.importJSON ./nix/nixpkgs-version.json;
     pinnedPkgs = hostPkgs.fetchFromGitHub {
       owner = "NixOS";
       repo = "nixpkgs-channels";
       inherit (pinnedVersion) rev sha256;
     };
  in import pinnedPkgs {config = { allowUnsupportedSystem = true; allowUnfree = true; }; };

  cp = pkgs.haskell.packages.${compiler}.callPackage;
in
  cp ./flare-timing.nix { }
