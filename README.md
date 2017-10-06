# Flare Timing

Flare Timing will show a pilot's competition score in detail but is currently a work in progress.

The official scoring program for hang gliding and paragliding competitions is [FS](http://fs.fai.org/). Its principal output for a competition task is an html table of pilot scores. As well as the day's score, the time on course and distance is shown as is the top level components of the score, typically lead points, time points, arrival points and position points.

Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding Commission) is an Air Sport Commission (ASC) of the Fédération Internationale Aéronautique (FAI). CIVL produce FS. It is the work of paid and volunteer developers.

FSDB is the database of FS, an XML format for inputs, working and outputs of scoring. In the C# implementation of FS, code that works at the highest level is mixed in with code that reads and writes directly to the FSDB file.

The scoring method is [well documented](http://fs.fai.org/trac/wiki/ScoringFormulas). Principal documents are;

* [Sporting Code Section 7A - Annex GAP](http://www.fai.org/downloads/civl/SC7A_GAP) - The definitive guide produced and maintained by CIVL.
* [GAP 2002](http://fs.fai.org/trac/raw-attachment/wiki/ScoringFormulas/GAP02_en.pdf) - A much shorter and less formal guide that is a good first read to gain an understanding of why GAP is needed and how it works.

Flare Timing provides a reference implementation of GAP and shows the working of how a pilot's score is calculated.

## Building

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
    
## Running

### Test Command Line Apps

Use these for testing that the inputs can be parsed;

#### KML Parser

    $ __shake-build/test-kml-parser --help
    A parser of KML, the Keyhole Markup Language, an XML format.
    
    test-kml-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the KML files in this directory
      -f --file=ITEM        With this one KML file
    
### IGC Parser

    $ __shake-build/test-igc-parser --help
    A parser for IGC, a plain-text file format from the International Gliding
    Commission for recording flights.

    test-igc-parser.exe [OPTIONS]

    Common flags:
      -d --dir=ITEM         Over all the IGC files in this directory
      -f --file=ITEM        With this one IGC file
      
### FSDB Parser

    $ __shake-build/test-fsdb-parser --help
    Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding
    Commission) is an Air Sport Commission (ASC) of the Fédération Internationale
    Aéronautique (FAI). CIVL produce FS, the official software for scoring hang
    gliding and paragliding competitions. FSDB is the database of FS, an XML format
    for inputs, working and outputs of scoring.

    fsdb-parser is a parser for a subset of the FSDB, just enough to cover the
    inputs of scoring.

    test-fsdb-parser [OPTIONS]

    Source:
         --dir=ITEM                 Over all the FSDB files in this directory
      -f --file=ITEM                With this one FSDB file
    Filter:
         --detail=tasks | nominals  Focus on these details
         
## Scoring Command Line Apps

Scoring is done with a pipeline of command line apps.

1. Extract the scoring inputs from FSDB, `FSDB -> YAML`.
2. Mask the flown tracks with the task zones.

### Inputs Extraction

    $ __shake-build/comp-xml-to-yaml --help
    Convert FSDB (XML) to YAML with only the inputs needed for scoring.

    comp-xml-to-yaml [OPTIONS]

    Source:
      -d --dir=ITEM   Over all the competition FSDB files in this directory
      -f --file=ITEM  With this one competition FSDB file
      
### Mask Tracks

    $ __shake-build/mask-tracks --help
    Given a competition YAML file and relative track log KML files, by masking the
    track logs with the zones, work out;
    * if the pilot launched
    * if they made goal then
        * how long the pilot took to reach goal
    * if they landed out then
        * how far they got along the course
        * how far yet to reach goal

    mask-tracks [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -r --reckon=RECKON NAME  Work out one of these things,
                               launch|goal|zones|goaldistance|flowndistance|time|lead

## Web Apps

### Comp Server

    $ __shake-build/comp-serve --help
    Serve nominals, tasks and pilots from a competition YAML file.

    comp-serve [OPTIONS]
      With one competition *.comp.yaml file supplied

    Common flags:
      -f --file=ITEM
      
Run the server;

    $ __shake-build/comp-serve --file=Forbes2012.comp.yaml
    Drive {file = "Forbes2012.comp.yaml"}
    ServeOptions {file = "Forbes2012.comp.yaml"}
    listening on port 3000

The following are the web service endpoints;

    http://localhost:3000/nominals
    http://localhost:3000/tasks
    http://localhost:3000/pilots

### Comp Client

Start the webpack devserver and navigate to `http://localhost:9000/app.html` for the client;

    ./build.sh view-start
    
    $ webpack-dev-server
    Project is running at http://localhost:9000/
