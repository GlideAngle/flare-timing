let
  config = {
    packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          ghc-tcplugins-extra =
            self.callPackage ../nix/ghc-tcplugins-extra.nix { };
          tasty =
            self.callPackage ../nix/tasty.nix { };
          uom-plugin =
            self.callPackage ../nix/uom-plugin.nix { };
          detour-via-uom  =
            self.callPackage ../detour-via-uom/default.nix { };
          flight-units =
            self.callPackage ../units/default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { compiler ? "ghc822"
  }:
  pkgs.haskell.packages.${compiler}.callPackage
  ./flight-latlng.nix
  { }
