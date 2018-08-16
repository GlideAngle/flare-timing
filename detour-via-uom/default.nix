let
  config = {
    packageOverrides = super: let self = super.pkgs; in
    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          ghc822 = super.haskell.packages.ghc822.override {
            overrides = self: super: {
              ghc-tcplugins-extra =
                self.callPackage ../nix/ghc-tcplugins-extra.nix { };
              tasty =
                self.callPackage ../nix/tasty.nix { };
              uom-plugin =
                self.callPackage ../nix/uom-plugin.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { compiler ? "ghc822"
  }:
  pkgs.haskell.packages.${compiler}.callPackage
  ./detour-via-uom.nix
  { }
