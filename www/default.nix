let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { www-flare-timing = pkgs.haskellPackages.www-flare-timing; }
