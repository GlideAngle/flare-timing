### Crossing Zones

    $ __shake-build/cross-zone --help

    Finds pairs of track fixes that cross each task control zone.

    Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
    competition inputs and k is a folder path specified in the inputs for
    tracklogs, one per task;
        Reads  ./c.comp-input.yaml
        Reads  ./k/p.kml
        Writes ./c.cross-zone.yaml

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed.

    The same thing goes if a list of pilots is supplied or not.

    cross-zone [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
