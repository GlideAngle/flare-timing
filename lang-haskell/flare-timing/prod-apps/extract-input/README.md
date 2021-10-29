### Extracting Scoring Inputs from FSDB

```
> ft-extract-input --help

Extracts just the inputs needed for scoring a competition.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.fsdb
    Writes ./c.comp-inputs.yaml

The *.fsdb does not hold zone tolerances or give and these must be supplied.
Note that the give fraction is required but the give distance is optional.

ft-extract-input [OPTIONS]

Source:
     --file=ITEM               With this one competition FSDB file
Give when crossing zones:
     --give-fraction=NUM       How much give as a fraction, 0.005 or 0.0001?
     --give-distance=NUM       How much give in metres?
Earth math:
  -p --pythagorus              Pythagorus method on a plane
  -h --haversines              Haversines on a sphere
  -v --vincenty                Vincenty method on an ellipsoid
  -a --andoyerlambert          Andoyer-Lambert method on an ellipsoid
     --forsytheandoyerlambert  Forsythe-Andoyer-Lambert method on an
                               ellipsoid
     --fsandoyer               The same formulation of Andoyer that FS uses
  -? --help                    Display help message
  -V --version                 Print version information
```
