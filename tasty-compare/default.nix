let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { tasty-compare = pkgs.haskellPackages.tasty-compare; }
