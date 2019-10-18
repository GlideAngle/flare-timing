While I could use a pinned version of nixpkgs, with vernix I can side-step some
problems by pinning and tweaking packages. I can also jail break and skip the
docs or testing if need be.

If I want to use the pinned nixpkgs then that setup is; pkgs = import
../nix/nixpkgs.nix { inherit config; };
