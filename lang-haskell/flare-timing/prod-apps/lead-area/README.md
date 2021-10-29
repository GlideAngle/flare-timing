### Leading Area

```
> ft-lead-area --help

From competition inputs '.comp-input.yaml', finds the aligned time '.csv' files
for each pilot. Keeping only tick and distance columns, initial zero distance
rows are dropped and rows that get further from goal are also dropped.

Where 'p' is the pilot name and '.' is the folder with competition inputs;
    Reads  ./flare-timing/align-time/task-n/p.csv
    Writes ./flare-timing/discard-further/task-n/p.csv

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.

ft-lead-area [OPTIONS]

Source:
  -f --file=ITEM                 With this one competition *.comp.yaml file
Filter:
  -t --task[=TASK NUMBER]        Which tasks?
  -p --pilot[=PILOT NAME]        Which pilots?
  -s --speedsectiononly          Exclude legs outside of the speed section?
  -? --help                      Display help message
  -V --version                   Print version information
Precision:
  -m --math[=rational|floating]  Do math with which kind of numbers?
```
