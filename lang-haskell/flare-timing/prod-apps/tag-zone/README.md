### Zone Taggings

```
> ft-tag-zone --help

For each crossing, given as a pair of fixes, interpolates the time and place
where it touches the control zone.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.cross-zone.yaml
    Writes ./c.tag-zone.yaml

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.

ft-tag-zone [OPTIONS]

Source:
  -f --file=ITEM                 With this one competition *.comp-input.yaml
                                 file
Filter:
  -t --task[=TASK NUMBER]        Which tasks?
  -p --pilot[=PILOT NAME]        Which pilots?
  -s --speedsectiononly          Exclude legs outside of the speed section?
  -? --help                      Display help message
  -V --version                   Print version information
Precision:
  -m --math[=rational|floating]  Do math with which kind of numbers?
```
