### Task Length

    $ __shake-build/task-length --help
    
    Works out the task length by following an optimal route.

    Where 'c' is the comp name and '.' is the folder with competition inputs;
        Reads  ./c.comp-inputs.yaml
        Writes ./c.task-length.yaml

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
