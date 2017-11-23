### Filter Times

    $ __shake-build/filter-time --help
    From competition inputs '.comp-input.yaml' and relative track logs '.kml',
    writes the distance to goal for every track log fix. These are collected in
    a single '.csv' per-pilot per-task. These are grouped into a single folder
    per-task as ./flare-timing/align-time/task-n/*.csv.

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed. The same thing goes if a list of pilots is supplied or
    not.

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
