### Align Times

    $ __shake-build/unpack-track --help

    From competition inputs '.comp-input.yaml', relative track logs '.kml' and
    tagged zones '.tag-zone.yaml', writes each fix to a single '.csv' per-pilot
    per-task, grouped into a single folder per-task.

    Dumps the track as a CSV.

    Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
    competition inputs and k is a folder path specified in the inputs for
    tracklogs, one per task;
        Reads  ./c.comp-input.yaml
        Reads  ./c.tag-zone.yaml
        Reads  ./k/p.kml
        Writes ./flare-timing/unpack-track/task-n/p.csv

    unpack-track [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
