### Task Length

    $ __shake-build/task-length --help
    Given a competition input YAML file, *.comp-input.yaml, work out the task
    length by flowing an optimal route.

    task-length [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
         --no-task-waypoints   Exclude the task waypoints?
