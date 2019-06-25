### Extracting Scoring Inputs

    $ __shake-build/extract-input --help

    Extracts just the inputs needed for scoring a competition.

    Where 'c' is the comp name and '.' is the folder with competition inputs;
        Reads  ./c.fsdb
        Writes ./c.comp-inputs.yaml 

    extract-input [OPTIONS]

    Source:
      -d --dir=ITEM   Over all the competition FSDB files in this directory
      -f --file=ITEM  With this one competition FSDB file
