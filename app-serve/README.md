# Flare Timing's Data Server

This internally named `app-serve` project is
a [servant](https://haskell-servant.readthedocs.io) web app.

To install it run `stack install app-serve` from the root of this repository.
Note the `local-bin-path: __shake-build` setting in stack's `stack.yaml`
configuration. The install app is named `comp-serve`.

To serve a competition that has already been scored. Go to the root folder of
the competition passing in the common base name as the `--file` option. These
are the set of the files with the inputs, workings and outputs of scoring:

```
> comp-serve --file=forbes2018
```

A scored competition will have a set of files the server reads its data from:

```
> ls -1
forbes2018.comp-input.yaml
forbes2018.cross-zone.yaml
forbes2018.fsdb
forbes2018.gap-point.yaml
forbes2018.land-out.yaml
forbes2018.mask-track.yaml
forbes2018.tag-zone.yaml
forbes2018.task-length.yaml
task 1
task 2
task 3
task 4
task 5
task 6
task 7
```
