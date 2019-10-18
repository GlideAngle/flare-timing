let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  { flight-fsdb = pkgs.haskellPackages.flight-fsdb; }
