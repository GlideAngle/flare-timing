### Trims the FSDB

```
> fs-trim --help

Filters a competition *.fsdb file keeping only the XML nodes needed for scoring
comparison.

fs-trim [OPTIONS]

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
