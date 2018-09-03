let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { detour-via-sci = pkgs.haskellPackages.detour-via-sci; }
