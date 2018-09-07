let
  config = {
    packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          ghc-tcplugins-extra =
            self.callPackage ../nix/ghc-tcplugins-extra.nix { };
          hcoord =
            self.callPackage ../nix/hcoord.nix { };
          hcoord-utm =
            self.callPackage ../nix/hcoord-utm.nix { };
          tasty =
            self.callPackage ../nix/tasty.nix { };
          uom-plugin =
            self.callPackage ../nix/uom-plugin.nix { };
          flight-earth =
            self.callPackage ../earth/default.nix { };
          flight-latlng =
            self.callPackage ../latlng/default.nix { };
          flight-task =
            self.callPackage ../task/default.nix { };
          flight-units =
            self.callPackage ../units/default.nix { };
          flight-zone =
            self.callPackage ../zone/default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { compiler ? "ghc822"
  }:
  pkgs.haskell.packages.${compiler}.callPackage
  ./flight-route.nix
  { }
