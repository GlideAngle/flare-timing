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
          detour-via-uom  =
            self.callPackage ../detour-via-uom/default.nix { };
          flight-latlng =
            self.callPackage ../latlng/default.nix { };
          flight-units =
            self.callPackage ../units/default.nix { };
          flight-zone =
            self.callPackage ../zone/default.nix { };
          tasty-compare =
            self.callPackage ../tasty-compare/default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { compiler ? "ghc822"
  }:
  pkgs.haskell.packages.${compiler}.callPackage
  ./flight-earth.nix
  { }
