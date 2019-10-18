let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { detour-via-uom = pkgs.haskellPackages.detour-via-uom; }
