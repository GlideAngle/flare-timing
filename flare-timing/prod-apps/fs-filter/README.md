### fs-filter

This console app reads an `*.fsdb` file and writes `*.clean-fsdb.xml` and
`*.trim-fsdb.xml` files. The clean file is stripped of sensitive personal
information such as birthdays and comments so that it may be published. The
trimmed file leaves only the elements and attributes used by flare-timing as
inputs to scoring or outputs that are used in comparisons.

    $ fs-filter --help

    Filters a competition *.fsdb file keeping only the XML nodes needed for scoring
    inputs or scoring output comparison.
