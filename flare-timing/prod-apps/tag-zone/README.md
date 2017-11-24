### Tagging Zones

    $ __shake-build/tag-zone --help
    Interpolate between crossing fixes '.cross-zone.yaml' for the time and place
    where a track tags a zone.

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed.

    The same thing goes if a list of pilots is supplied or not.

    tag-zone [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
