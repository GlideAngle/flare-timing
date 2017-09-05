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
    
## Running

### Test Command Line Apps

Use these for testing that the inputs can be parsed;

#### KML Parser

    $ __shake-build/kml-parser --help
    Flight KML Parser 0.1.0

    Parsing flight KML files.
    
    kml-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the files in this directory
      -f --file=ITEM        With this one file
    
### IGC Parser

    $ __shake-build/igc-parser --help
    Flight IGC Parser 0.1.0

    Parsing flight IGC files.

    igc-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the files in this directory
      -f --file=ITEM        With this one file
      
### FSDB Parser

    $ __shake-build/fsdb-parser --help
    Flight Scoring Database Parser 0.1.0

    Parsing flight fsdb files.

    fsdb-parser.exe [OPTIONS]

    Source:
         --dir=ITEM                 Over all the files in this directory
      -f --file=ITEM                With this one file
    Filter:
         --detail=tasks | nominals  Focus on these details

### Web App

Start the webpack devserver and navigate to `http://localhost:9000/app.html`;

    ./build.sh view-start
    
    $ webpack-dev-server
    Project is running at http://localhost:9000/
