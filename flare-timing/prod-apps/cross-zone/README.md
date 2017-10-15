### Crossing Zones

    $ __shake-build/cross-zone --help
    From competition inputs '.comp-input.yaml' and relative track logs '.kml', find
    pairs of fixes crossing over a zone.

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed.

    The same thing goes if a list of pilots is supplied or not.

    <interactive> [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
