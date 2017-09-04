# Flare Timing

Each folder holds a separate package, each with their own [stack](https://docs.haskellstack.org) and [nix-build](https://nixos.org/nix/manual/#sec-building-simple) setup;

* `./build` for building with [shake](http://shakebuild.com/).
* `./flare-timing` command line programs for scoring and a web view of the results.
* `./comp`, package `flight-comp`, a subset of `fsdb` inputs and mask outputs.
* `./fsdb`, package `flight-fsdb` for parsing the FS database, an XML format.
* `./gap`, package `flight-gap` for GAP scoring.
* `./igc`, package `flight-igc` parsing IGC files.
* `./kml`, package `flight-kml` parsing KML files.
* `./mask`, package `flight-mask` masks a flight task with a pilot's flown track.
* `./task`, package `flight-task` shortest distance path to fly a task.
* `./track`, package `flight-track` reading flight track logs.
* `./units`, package `flight-units` defines units of measure used in scoring.
* `siggy-chardust` for rounding to decimal places and significant digits.
