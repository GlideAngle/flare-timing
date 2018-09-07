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
          flight-comp =
            self.callPackage ../comp/default.nix { };
          flight-gap =
            self.callPackage ../gap/default.nix { };
          flight-latlng =
            self.callPackage ../latlng/default.nix { };
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
  ./flight-fsdb.nix
  { }
