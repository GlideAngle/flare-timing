### fs-filter

This console app reads an `*.fsdb` file and writes `*.clean-fsdb.xml` and
`*.trim-fsdb.xml` files. The clean file is stripped of sensitive personal
information such as birthdays and comments so that it may be published. The
trimmed file leaves only the elements and attributes used by flare-timing as
inputs to scoring or outputs for comparison.

Extract the inputs for scoring with [extract-input](../extract-input). To
prepare for comparison on the web app, select routes with
[fs-route](../fs-route), the land-outs with [fs-effort](../fs-effort) and the
scores with [fs-score](../fs-score). Though the `*.fsdb` is a format of FS, any
other scoring program that can generate the `*.trim-fsdb.xml` subset of
elements and attributes can be scored again with flare-timing and the closeness
of scores between it and flare-timing can be compared with the [flare-timing,
the web app](../../../app-view) run locally or hosted as a static web site.

    $ fs-filter --help

    Filters a competition *.fsdb file keeping only the XML nodes needed for scoring
    inputs or scoring output comparison.
