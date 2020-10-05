### Masking Tracks

    $ ft-mask-track --help

    By masking the track logs with the zones, works out;
    * if the pilot launched
    * if they made goal then
        * how long the pilot took to reach goal
    * if they landed out then
        * how far they got along the course
        * how far yet to reach goal

    Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
    competition inputs and k is a folder path specified in the inputs for
    tracklogs, one per task;
        Reads  ./c.comp-input.yaml
        Reads  ./k/p.kml
        Reads  ./c.tag-zone.yaml
        Writes ./flare-timing/discard-further/task-n/p.csv
        Writes ./c.mask-track.yaml

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed. The same thing goes if a list of pilots is supplied or
    not.    

    ft-mask-track [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -r --reckon=RECKON NAME  Work out one of these things,
                               launch|goal|zones|goaldistance|flowndistance|time|lead
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
