### Task Lengths

```
> ft-task-length --help

Works out the task length by following an optimal route.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.comp-inputs.yaml
    Writes ./c.task-length.yaml

ft-task-length [OPTIONS]

Source:
     --file=ITEM               With this one competition *.comp.yaml file
Filter:
  -t --task[=TASK NUMBER]      Which tasks?
  -m --measure=METHOD          Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges
     --no-task-waypoints       Exclude the task waypoints?
Earth math:
  -p --pythagorus              Pythagorus method on a plane
  -h --haversines              Haversines on a sphere
  -v --vincenty                Vincenty method on an ellipsoid
  -a --andoyerlambert          Andoyer-Lambert method on an ellipsoid
     --forsytheandoyerlambert  Forsythe-Andoyer-Lambert method on an
                               ellipsoid
  -? --help                    Display help message
  -V --version                 Print version information
```
