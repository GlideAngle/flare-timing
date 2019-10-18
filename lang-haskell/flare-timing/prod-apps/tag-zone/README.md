### Tagging Zones

    $ __shake-build/tag-zone --help

    For each crossing, given as a pair of fixes, interpolates the time and place
    where it touches the control zone.

    Where 'c' is the comp name and '.' is the folder with competition inputs;
        Reads  ./c.cross-zone.yaml
        Writes ./c.tag-zone.yaml

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed. The same thing goes if a list of pilots is supplied or
    not.

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
