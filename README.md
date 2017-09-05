# Flare Timing

## Building

Each library package can be built standalone. For example, the `flight-units` package can be built with [nix-build](https://nixos.org/nix/manual/#sec-building-simple) after having setup the [overlay](https://github.com/BlockScope/nix-config).

    ln -s overlay.nix ~/.config/nixpkgs/overlay
    nix-build "<nixpkgs>" -A haskellPackages.flight-units
    
The same package can be built with [stack](https://docs.haskellstack.org);

    cd units
    stack build    

* `./comp`, package `flight-comp`, a subset of `fsdb` inputs and mask outputs.
* `./fsdb`, package `flight-fsdb` for parsing the FS database, an XML format.
* `./gap`, package `flight-gap` for GAP scoring.
* `./igc`, package `flight-igc` parsing IGC files.
* `./kml`, package `flight-kml` parsing KML files.
* `./mask`, package `flight-mask` masks a flight task with a pilot's flown track.
* `./task`, package `flight-task` shortest distance path to fly a task.
* `./track`, package `flight-track` reading flight track logs.
* `./units`, package `flight-units` defines units of measure used in scoring.
* `./siggy-chardust`, package `siggy-chardust` for rounding to decimal places and significant digits.

Kept together in `./flare-timing` are the command line programs for scoring and a web view of the results.

The shake targets and rules are in `./build`. To build all packages in nix;

    ./build.sh nix
