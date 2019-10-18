# Testing Flare Timing

Many library packages have their own test suites and the following command line
apps can be used to test the parsing of `*.fsdb`, `*.igc` and `*.kml` files by
dumping what is parsed to stdout.

```
    $ test-kml-parser --help
    A parser of KML, the Keyhole Markup Language, an XML format.

    $ test-igc-parser --help
    A parser for IGC, a plain-text file format from the International Gliding
    Commission for recording flights.

    $ test-fsdb-parser --help
    Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding
    Commission) is an Air Sport Commission (ASC) of the Fédération Internationale
    Aéronautique (FAI). CIVL produce FS, the official software for scoring hang
    gliding and paragliding competitions. FSDB is the database of FS, an XML format
    for inputs, working and outputs of scoring.

    fsdb-parser is a parser for a subset of the FSDB, just enough to cover the
    inputs of scoring.
```
