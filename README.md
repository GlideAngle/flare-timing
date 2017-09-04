# Flare Timing

Each folder holds a separate package, each with their own [stack](https://docs.haskellstack.org) and [nix-build](https://nixos.org/nix/manual/#sec-building-simple) setup;

* `flare-timing` command line programs for scoring and a web view of the results.
* `flight-comp` the subset of `fsdb` inputs and mask outputs.
* `flight-fsdb` parsing the FS database, an XML format.
* `flight-gap` the guts of GAP scoring.
* `flight-igc` parsing IGC files.
* `flight-kml` parsing KML files.
* `flight-mask` masks a flight task with a pilot's flown track.
* `flight-task` how far is a task?
* `flight-track` reading flight track logs.
* `flight-units` defines units of measure used in scoring.
* `siggy-chardust` for rounding to decimal places and significant digits.
