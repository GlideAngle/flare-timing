# Testing Flare Timing

In addition to each library package's own test suite there command
line apps that can be use to parse `FSDB`, `IGC` and `KML` files and dump
results to stdout.

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
