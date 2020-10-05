### Align Times

    $ ft-align-time --help

    From competition inputs '.comp-input.yaml', relative track logs '.kml' and
    tagged zones '.tag-zone.yaml', writes each fix to a single '.csv' per-pilot
    per-task, grouped into a single folder per-task.

    Using the tagged zones to find the first crossing of a pilot into the speed
    section, three columns are added to the CSV;
        leg        The leg of the speed section of the task
        tick       The time in seconds from the first crossing
        distance   The distance to goal

    Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
    competition inputs and k is a folder path specified in the inputs for
    tracklogs, one per task;
        Reads  ./c.comp-input.yaml
        Reads  ./c.tag-zone.yaml
        Reads  ./k/p.kml
        Writes ./flare-timing/align-time/task-n/p.csv

    ft-align-time [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
