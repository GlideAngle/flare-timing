# SEE: https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md
# SEE: https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
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
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { compiler ? "ghc822"
  }:
  pkgs.haskell.packages.${compiler}.callPackage
  ./flight-units.nix
  { }
