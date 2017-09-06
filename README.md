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
    A parser of KML, the Keyhole Markup Language, an XML format.
    
    kml-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the KML files in this directory
      -f --file=ITEM        With this one KML file
    
### IGC Parser

    $ __shake-build/igc-parser --help
    A parser for IGC, a plain-text file format from the International Gliding
    Commission for recording flights.

    igc-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the IGC files in this directory
      -f --file=ITEM        With this one IGC file
      
### FSDB Parser

    $ __shake-build/fsdb-parser --help
    Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding
    Commission) is an Air Sport Commission (ASC) of the Fédération Internationale
    Aéronautique (FAI). CIVL produce FS, the official software for scoring hang
    gliding and paragliding competitions. FSDB is the database of FS, an XML format
    for inputs, working and outputs of scoring.

    fsdb-parser is a parser for a subset of the FSDB, just enough to cover the
    inputs of scoring.

    fsdb-parser [OPTIONS]

    Source:
         --dir=ITEM                 Over all the FSDB files in this directory
      -f --file=ITEM                With this one FSDB file
    Filter:
         --detail=tasks | nominals  Focus on these details

### Web App

Start the webpack devserver and navigate to `http://localhost:9000/app.html`;

    ./build.sh view-start
    
    $ webpack-dev-server
    Project is running at http://localhost:9000/
