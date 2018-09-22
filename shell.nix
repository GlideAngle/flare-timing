let
  config = import ./nix/config.nix {};
  pkgs = import ./nix/nixpkgs.nix { inherit config; };
in
  import siggy-chardust/drv.nix { nixpkgs = pkgs; }
