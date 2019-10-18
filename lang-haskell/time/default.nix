let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { flight-time = pkgs.haskellPackages.flight-time; }
