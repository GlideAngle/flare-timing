let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { siggy-chardust = pkgs.haskellPackages.siggy-chardust; }
