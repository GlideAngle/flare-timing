### Faring Out

```
> ft-far-out --help

Group and count far outs.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.mask-track.yaml
    Writes ./c.far-out.yaml

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed.

The same thing goes if a list of pilots is supplied or not.

ft-far-out [OPTIONS]

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
