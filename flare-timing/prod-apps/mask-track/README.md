### Masking Tracks

    $ __shake-build/mask-track --help
    Given a competition input YAML file, *.comp-input.yaml, and relative track log
    KML files, by masking the track logs with the zones, work out;
    * if the pilot launched
    * if they made goal then
        * how long the pilot took to reach goal
    * if they landed out then
        * how far they got along the course
        * how far yet to reach goal

    mask-track [OPTIONS]

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
