# Building Flare Timing

Each library package can be built standalone. For example, the `flight-units` package can be built with [nix-build](https://nixos.org/nix/manual/#sec-building-simple) after having setup the [overlay](https://github.com/BlockScope/nix-config).

    ln -s overlay.nix ~/.config/nixpkgs/overlay
    nix-build "<nixpkgs>" -A haskellPackages.flight-units
    
The same package can be built with [stack](https://docs.haskellstack.org);

    cd units
    stack build
    
The following package dependencies are included in this repository; [`flight-comp`](comp)
[`flight-fsdb`](fsdb)
[`flight-gap`](gap)
[`flight-igc`](igc)
[`flight-kml`](kml)
[`flight-mask`](mask)
[`flight-task`](task)
[`flight-track`](track)
[`flight-units`](units)
 and [`siggy-chardust`](siggy-chardust). In `./flare-timing` are command line programs for scoring and a web view of the results.

The shake targets and rules are in `./build`. To build all packages in nix;

    ./build.sh nix
    
