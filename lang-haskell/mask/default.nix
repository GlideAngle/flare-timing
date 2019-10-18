let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { flight-mask = pkgs.haskellPackages.flight-mask; }
