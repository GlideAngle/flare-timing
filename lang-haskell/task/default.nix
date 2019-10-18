let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { flight-task = pkgs.haskellPackages.flight-task; }
